import os
import subprocess
import sys
import threading
import shutil
import numpy as np
from PyQt5 import QtWidgets
from PyQt5.QtWidgets import QMessageBox
from matplotlib.backends.backend_qt5 import NavigationToolbar2QT as NavigationToolbar
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.figure import Figure
from sympy import roots
from sympy.abc import x

from fractureSH_gui import Ui_MainWindow


class MyFirstGuiProgram(Ui_MainWindow):
    def __init__(self, dialog):
        Ui_MainWindow.__init__(self)
        self.setupUi(dialog)

        ###Cria o layout para plotagem
        # figura Tab1
        self.fig = Figure(figsize=(8,3),facecolor='white')
        self.fig.subplots_adjust(hspace= 0.40, wspace= 0.60,left=0.10, right=0.98, top=0.88, bottom=0.17)
        self.canvas = FigureCanvas(self.fig)
        self.canvas.setParent(self.widget)
        layout = QtWidgets.QVBoxLayout()
        self.widget.setLayout(layout)
        layout.addWidget(self.canvas)
        self.mpl_toolbar = NavigationToolbar(self.canvas, self.widget)
        self.fig.text(0.5, 0.1, 'Geofone', va='center')
        self.fig.text(0.02, 0.33, 'Tempo(s)', va='center', rotation='vertical')
        self.fig.text(0.45, 0.5, 'Ângulo de incidência (graus)', va='center', size= 8)
        self.fig.text(0.02, 0.73, 'Coeficiente de reflexão', va='center', rotation='vertical', size=7)
        self.axes = self.fig.add_subplot(211)
        self.axes2 = self.axes.twiny()
        self.axes.grid()
        self.axes_time = self.fig.add_subplot(212)
        self.axes.tick_params(labelsize=6)
        self.axes2.tick_params(labelsize=6)
        self.axes_time.tick_params(labelsize=6)
        self.axes_time.grid()


        #figura tab2
        self.fig_anray = Figure(figsize=(9,6), facecolor='white')
        self.fig_anray2 = Figure(figsize=(9, 6), facecolor='white')
        self.fig_anray.text(0, 0.6, 'Coeficiente de reflexão', va='center', rotation='vertical')
        self.fig_anray.text(0.985, 0.6, 'Separação', va='center', rotation='vertical')
        self.fig_anray.text(0.5, 0.12, 'Geofone', va='center')
        self.fig_anray2.text(0, 0.6, 'Coeficiente de reflexão', va='center', rotation='vertical')
        self.fig_anray2.text(0.5, 0.12, 'Geofone', va='center')
        self.canvas_anray = FigureCanvas(self.fig_anray)
        self.canvas_anray2 = FigureCanvas(self.fig_anray2)
        self.canvas_anray.setParent(self.widget_anray)
        self.canvas_anray2.setParent(self.widget_anray2)
        layout = QtWidgets.QVBoxLayout()
        layout2 = QtWidgets.QVBoxLayout()
        self.widget_anray.setLayout(layout)
        layout.addWidget(self.canvas_anray)
        self.widget_anray2.setLayout(layout2)
        layout2.addWidget(self.canvas_anray2)
        self.mpl_toolbar = NavigationToolbar(self.canvas_anray, self.widget_anray)
        self.mpl_toolbar2 = NavigationToolbar(self.canvas_anray2, self.widget_anray2)
        self.fig_anray.subplots_adjust(hspace=0.27, left=0.10, right=0.92, top=0.88, bottom=0.17)
        self.fig_anray2.subplots_adjust(hspace=0.27, left=0.10, right=0.98, top=0.93, bottom=0.17)
        #subplots
        self.axes_anray_tot = self.fig_anray.add_subplot(311)
        self.axes_anray2_tot = self.fig_anray2.add_subplot(311)
        self.axes_anray_tot2 = self.axes_anray_tot.twinx()
        self.axes_anray_tot.set_ylabel("total")
        self.axes_anray2_tot.set_ylabel("total")
        self.axes_anray2_rad = self.fig_anray2.add_subplot(312)
        self.axes_anray2_rad.set_ylabel("transversal")
        self.axes_anray_time = self.fig_anray.add_subplot(313)
        self.axes_anray2_time = self.fig_anray2.add_subplot(313)
        self.axes_anray_trans = self.fig_anray.add_subplot(312)
        self.axes_anray_trans.set_ylabel("transversal")
        self.axes_anray_trans2 = self.axes_anray_trans.twinx()

        self.axes_anray_time.set_ylabel('tempo')
        self.axes_anray2_time.set_ylabel('tempo')
        self.axes_anray_tot.grid()
        self.axes_anray_trans.grid()
        self.axes_anray2_rad.grid()
        self.axes_anray_time.grid()
        self.axes_anray2_tot.grid()
        self.axes_anray2_time.grid()
        self.axes_anray_tot.tick_params(labelsize=6)
        self.axes_anray_trans.tick_params(labelsize=6)
        self.axes_anray2_rad.tick_params(labelsize=6)
        self.axes_anray_tot2.tick_params(labelsize=6)
        self.axes_anray2_tot.tick_params(labelsize=6)
        self.axes_anray_trans2.tick_params(labelsize=6)
        ###

        #figura tab3
        self.fig_sismo = Figure(dpi=50, facecolor='white')
        self.canvas_sismo = FigureCanvas(self.fig_sismo)
        self.canvas_sismo.setParent(self.widget_sismo)
        self.fig_sismo.subplots_adjust(wspace=0.11, left=0.05, right=0.98, top=0.93, bottom=0.10)
        layout = QtWidgets.QVBoxLayout()
        self.widget_sismo.setLayout(layout)
        layout.addWidget(self.canvas_sismo)
        self.axes_sismo_x = self.fig_sismo.add_subplot(111)
        self.mpl_toolbar = NavigationToolbar(self.canvas_sismo, self.widget_sismo)
        self.fig_sismo.text(0.48, 0.04, 'Distância (m)', va='center', size= 14)
        self.fig_sismo.text(0.01, 0.5, 'Tempo (s)', va='center', rotation='vertical', size= 14)
        self.fig_sismo.text(0.48, 0.96, 'Transversal', va='center', size= 14)
        # self.fig_sismo.text(0.75, 0.96, 'Vertical', va='center', size=14)

        #figura tab4
        self.fig_sismo2 = Figure(dpi=100, facecolor='white')
        self.canvas_sismo2 = FigureCanvas(self.fig_sismo2)
        self.canvas_sismo2.setParent(self.widget_sismo2)
        self.fig_sismo2.set_tight_layout(True)
        layout = QtWidgets.QVBoxLayout()
        self.widget_sismo2.setLayout(layout)
        layout.addWidget(self.canvas_sismo2)
        self.axes_sismo2_1 = self.fig_sismo2.add_subplot(211)
        self.axes_sismo2_2 = self.fig_sismo2.add_subplot(212)
        self.mpl_toolbar = NavigationToolbar(self.canvas_sismo2, self.widget_sismo2)



        ###Define os valores iniciais
        self.spinBox_vp1.setValue(2250)
        self.spinBox_vs1.setValue(1200)
        self.spinBox_p1.setValue(2100)
        self.spinBox_vp2.setValue(4500)
        self.spinBox_vs2.setValue(2500)
        self.spinBox_p2.setValue(2700)
        #Velocidades do modelo de Ruger(para teste)
        #self.spinBox_vp1.setValue(2433)
        #self.spinBox_vs1.setValue(1627)
        #self.spinBox_p1.setValue(2405)
        #self.spinBox_vp2.setValue(2690)
        #self.spinBox_vs2.setValue(1400)
        #self.spinBox_p2.setValue(2070)
        self.doubleSpinBox_aspect.setValue(0.01)
        self.spinBox_fract.setValue(5)
        self.doubleSpinBox_bulk.setValue(2.2)
        self.doubleSpinBox_shear.setValue(0)
        self.spinBox_thick.setValue(100)
        self.spinBox_ngeo.setValue(48)
        self.spinBox_rmin.setValue(20)
        self.spinBox_rstep.setValue(2)
        self.size = 0
        self.size_plot = 0
        self.time_basalto =0
        self.time_solo = 0
        self.refl_tot_0 = 0
        self.refl_tot_30 = 0
        self.refl_tot_45 = 0
        self.refl_tot_60 = 0
        self.refl_tot_90 = 0
        self.refl_x_0 = 0
        self.refl_x_30 = 0
        self.refl_x_45 = 0
        self.refl_x_60 = 0
        self.refl_x_90 = 0
        self.refl_y_0 = 0
        self.refl_y_30 = 0
        self.refl_y_45 = 0
        self.refl_y_60 = 0
        self.refl_y_90 = 0
        self.refl_z_0 = 0
        self.refl_z_30 = 0
        self.refl_z_45 = 0
        self.refl_z_60 = 0
        self.refl_z_90 = 0
        self.refl_solo_rad_0 = 0
        self.refl_solo_y_0 = 0
        self.refl_solo_z_0 = 0
        self.refl_solo_x_30 = 0
        self.refl_solo_y_30 = 0
        self.refl_solo_z_30 = 0
        self.refl_solo_x_45 = 0
        self.refl_solo_y_45 = 0
        self.refl_solo_z_45 = 0
        self.refl_solo_x_60 = 0
        self.refl_solo_y_60 = 0
        self.refl_solo_z_60 = 0
        self.refl_solo_x_60 = 0
        self.refl_solo_y_60 = 0
        self.refl_solo_z_60 = 0
        self.refl_solo_x_90 = 0
        self.refl_solo_y_90 = 0
        self.refl_solo_z_90 = 0
        self.solo_fase_rad = 0  #para o solo as fases são iguais em todos azimutes...
        self.solo_fase_z = 0
        self.hti_fase_rad_0 = 0
        self.hti_fase_rad_30 = 0
        self.hti_fase_rad_45 = 0
        self.hti_fase_rad_60 = 0
        self.hti_fase_rad_90 = 0
        self.hti_fase_z_0 = 0
        self.hti_fase_z_30 = 0
        self.hti_fase_z_45 = 0
        self.hti_fase_z_60 = 0
        self.hti_fase_z_90 = 0
        self.dn = 0
        self.dt = 0
        ###


        ###define as ações
        self.spinBox_vp1.valueChanged.connect(self.vp1)
        self.spinBox_vp2.valueChanged.connect(self.vp2)
        self.spinBox_vs1.valueChanged.connect(self.plot)
        self.spinBox_p1.valueChanged.connect(self.plot)
        self.spinBox_vp2.valueChanged.connect(self.weak_calc)
        self.spinBox_vs2.valueChanged.connect(self.weak_calc)
        self.spinBox_p2.valueChanged.connect(self.weak_calc)
        self.doubleSpinBox_aspect.valueChanged.connect(self.weak_calc)
        self.spinBox_fract.valueChanged.connect(self.slider_pos)
        self.doubleSpinBox_aspect.valueChanged.connect(self.slider_pos)
        self.doubleSpinBox_bulk.valueChanged.connect(self.weak_calc)
        self.doubleSpinBox_shear.valueChanged.connect(self.weak_calc)
        self.verticalSlider_fract.valueChanged.connect(self.weak_calc)
        self.verticalSlider_aspect.valueChanged.connect(self.slider_pos1)
        self.doubleSpinBox_DN.valueChanged.connect(self.slider_pos2)
        self.doubleSpinBox_DT.valueChanged.connect(self.slider_pos2)
        self.verticalSlider_DN.valueChanged.connect(self.slider_pos3)
        self.verticalSlider_DT.valueChanged.connect(self.slider_pos3)
        self.doubleSpinBox_d.valueChanged.connect(self.plot)
        self.doubleSpinBox_e.valueChanged.connect(self.plot)
        self.doubleSpinBox_y.valueChanged.connect(self.plot)
        self.spinBox_ngeo.valueChanged.connect(self.plot)
        self.spinBox_rmin.valueChanged.connect(self.plot)
        self.spinBox_rstep.valueChanged.connect(self.plot)
        self.spinBox_thick.valueChanged.connect(self.plot)
        self.split_box0_90.stateChanged.connect(self.plot)
        self.split_box_anray_0_90.stateChanged.connect(self.split)
        self.split_box_anray_0_45.stateChanged.connect(self.split)
        self.split_box_anray_30_60.stateChanged.connect(self.split)
        self.split_box_anray_45_90.stateChanged.connect(self.split)
        self.pushButton.clicked.connect(self.anray)
        self.checkBox_solo.pressed.connect(self.activate)
        self.checkBox_solo.released.connect(self.plot)
        self.pushButton_2.pressed.connect(self.plot)
        self.verticalSlider_aspect.valueChanged.connect(self.slider_pos1)
        self.sismo_button.clicked.connect(self.plot_sismograma)
        self.radioButton_0.toggled.connect(self.plot_sismograma_v)
        self.radioButton_30.toggled.connect(self.plot_sismograma_v)
        self.radioButton_45.toggled.connect(self.plot_sismograma_v)
        self.radioButton_60.toggled.connect(self.plot_sismograma_v)
        self.radioButton_90.toggled.connect(self.plot_sismograma_v)
        self.radioButton_plot_x.toggled.connect(self.plot_sismo_azim)
        self.radio_sismo_0_90.toggled.connect(self.plot_sismo_azim)
        self.radio_sismo_0_45.toggled.connect(self.plot_sismo_azim)
        self.radio_sismo_45_90.toggled.connect(self.plot_sismo_azim)
        self.radio_sismo_30_60.toggled.connect(self.plot_sismo_azim)
        self.checkBox_solo_sismo.clicked.connect(self.sismo_enable)
        self.az_tmin.valueChanged.connect(self.plot_sismo_azim)
        self.az_tmax.valueChanged.connect(self.plot_sismo_azim)
        self.slider_pos()
        self.anray_path = os.getcwd()
        if not os.path.exists('HTI_SH_model'):
            os.makedirs('HTI_SH_model')

    def vp1(self):
        vp = self.spinBox_vp1.value()
        vs = vp/np.sqrt(3)
        self.spinBox_vs1.setValue(vs)

    def vp2(self):
        vp = self.spinBox_vp2.value()
        vs = vp/np.sqrt(3)
        self.spinBox_vs2.setValue(vs)

    def message(self):
        msg = QMessageBox()
        msg.setIcon(QMessageBox.Warning)
        msg.setText("Erro")
        msg.setInformativeText("Certifique-se de gerar os arquivos e manter a opção (solo) correspondente na primeira aba.")
        msg.exec_()


    #Função para ativar a camada de solo nos cálculos
    def activate(self):
        if self.checkBox_solo.isChecked():
            self.solo_espessura.setDisabled(True)
            self.solo_vp.setDisabled(True)
            self.solo_vs.setDisabled(True)
            self.solo_densidade.setDisabled(True)
        else:
            self.solo_espessura.setEnabled(True)
            self.solo_vp.setEnabled(True)
            self.solo_vs.setEnabled(True)
            self.solo_densidade.setEnabled(True)
            self.pushButton_2.setEnabled(True)


    #Funções para ajustar spinbox e slider.

    def slider_pos(self):
        self.verticalSlider_fract.setValue(self.spinBox_fract.value())

    def slider_pos1(self):
        self.doubleSpinBox_aspect.setValue(self.verticalSlider_aspect.value() / 10000)

    def slider_pos2(self):
        self.verticalSlider_DN.setValue(self.doubleSpinBox_DN.value()*1000)
        self.verticalSlider_DT.setValue(self.doubleSpinBox_DT.value()*1000)

    def slider_pos3(self):
        self.doubleSpinBox_DN.setValue(self.verticalSlider_DN.value()/1000)
        self.doubleSpinBox_DT.setValue(self.verticalSlider_DT.value()/1000)

        self.aniso_parameters()

    #Função para calcular os parametros de fraqueza
    def weak_calc(self):

        self.doubleSpinBox_DN.valueChanged.disconnect(self.slider_pos2)
        self.doubleSpinBox_DT.valueChanged.disconnect(self.slider_pos2)
        self.verticalSlider_DN.valueChanged.disconnect(self.slider_pos3)
        self.verticalSlider_DT.valueChanged.disconnect(self.slider_pos3)

        #Ajusta o valor do spinbox de acordo com o slider
        self.spinBox_fract.setValue(self.verticalSlider_fract.value())
        self.verticalSlider_aspect.setValue(self.doubleSpinBox_aspect.value()*10000)


        # grau de fraturamento e aspect_ratio
        e = self.spinBox_fract.value() / 100
        a = self.doubleSpinBox_aspect.value()

        vp2 = self.spinBox_vp2.value()
        vs2 = self.spinBox_vs2.value()
        p2 = self.spinBox_p2.value()

        g = (vs2 ** 2) / (vp2 ** 2)

        # parametro de Lame
        mu = p2 * (vs2 ** 2)

        # bulk and shear modulus
        kl = self.doubleSpinBox_bulk.value() * 10 ** 9
        ul = self.doubleSpinBox_shear.value() * 10 ** 9

        # grau de fraturamento de Hudson. Obtido de Chen 2014 (2) e Bakulin 2000 (14)
        DN = 4 * e / (3 * g * (1 - g) * (1 + ((kl + (4 / 3) * ul) / (np.pi * (1 - g) * mu * a))))
        self.doubleSpinBox_DN.setValue(DN)
        self.verticalSlider_DN.setValue(DN*1000)

        DT= 16 * e / (3 * (3 - 2 * g) * (1 + ((4 * ul) / (np.pi * (3 - 2 * g) * mu * a))))
        self.doubleSpinBox_DT.setValue(DT)
        self.verticalSlider_DT.setValue(DT*1000)

        self.doubleSpinBox_DN.valueChanged.connect(self.slider_pos2)
        self.doubleSpinBox_DT.valueChanged.connect(self.slider_pos2)
        self.verticalSlider_DN.valueChanged.connect(self.slider_pos3)
        self.verticalSlider_DT.valueChanged.connect(self.slider_pos3)

        self.aniso_parameters()


    #Função que calcula os parametros de anisotropia
    def aniso_parameters(self):

        self.doubleSpinBox_d.valueChanged.disconnect(self.plot)
        self.doubleSpinBox_e.valueChanged.disconnect(self.plot)
        self.doubleSpinBox_y.valueChanged.disconnect(self.plot)

        vp2 = self.spinBox_vp2.value()
        vs2 = self.spinBox_vs2.value()
        p2 = self.spinBox_p2.value()

        DN_H = self.doubleSpinBox_DN.value()
        DT_H = self.doubleSpinBox_DT.value()

        # A partir de Chen 2014 e Bakulin 2000 (27)

        # parametros de Lame
        lamb = p2 * (vp2 ** 2 - 2 * (vs2 ** 2))
        mu = p2 * (vs2 ** 2)

        M = lamb + 2 * mu
        r = lamb / M

        c11 = M * (1 - DN_H)
        c33 = M * (1 - (r ** 2) * DN_H)
        c13 = lamb * (1 - DN_H)
        c44 = mu
        c66 = mu * (1 - DT_H)
        c55 = c66
        c23 = c33 - 2 * c44
        self.c11 = (c11/p2)/1000000
        self.c13 = (c13/p2)/1000000
        self.c23 = (c23/p2)/1000000
        self.c33 = (c33/p2)/1000000
        self.c44 = (c44/p2)/1000000
        self.c55 = (c55 /p2)/1000000

        #Para imprimir os parâmetros elásticos, descomentar as linhas abaixo.
        # print('A11=', c11/p2)
        # print('A13=', c13/p2)
        # print('A23=', c23/p2)
        # print('A33=', c33/p2)
        # print('A44=', c44/p2)
        # print('A55=', c55/p2)
        self.dn = DN_H
        self.dt = DT_H


        e2_v = (c11 - c33) / (2 * c33)
        self.doubleSpinBox_e.setValue(abs(e2_v))


        d2_v = (((c13 + c55) ** 2) - ((c33 - c55) ** 2)) / (2 * c33 * (c33 - c55))
        self.doubleSpinBox_d.setValue(abs(d2_v))

        y2_v = (c66 - c44) / (2 * c44)
        self.doubleSpinBox_y.setValue(abs(y2_v))

        self.doubleSpinBox_d.valueChanged.connect(self.plot)
        self.doubleSpinBox_e.valueChanged.connect(self.plot)
        self.doubleSpinBox_y.valueChanged.connect(self.plot)

        self.plot()

    #Função que realiza a plotagem principal
    def plot(self):

        self.axes.cla()
        self.axes_time.cla()

        # Parametros do meio superior(1)
        vp1 = self.spinBox_vp1.value()
        vs1 = self.spinBox_vs1.value()
        p1 = self.spinBox_p1.value()


        # Parametros do meio inferior(2)
        vp2 = self.spinBox_vp2.value()
        vs2 = self.spinBox_vs2.value()
        p2 = self.spinBox_p2.value()


        vs2p = np.sqrt(self.c55 * 1000000)

        # Impedância vertical
        Z1 = p1 * vs1
        Z2 = p2 * vs2

        # Módulo de cisalhamento
        G1 = p1 * pow(vs1, 2)
        G2 = p2 * pow(vs2, 2)

        # diferenças e médias
        deltaZ = Z2 - Z1
        medZ = (Z1 + Z2) / 2
        deltap = p2 - p1
        medp = (p1 + p2) / 2
        deltavs = vs2 - vs1
        medvs = (vs1 + vs2) / 2
        deltavs2 = vs2p - vs1
        medvs2 = (vs1 + vs2p) / 2
        deltavp = vp2 - vp1
        medvp = (vp1 + vp2) / 2
        deltad = -self.doubleSpinBox_d.value()
        deltae = -self.doubleSpinBox_e.value()
        deltay = self.doubleSpinBox_y.value()


        rmin = self.spinBox_rmin.value()
        rstep = self.spinBox_rstep.value()
        thick = self.spinBox_thick.value()


        # ângulo de incidência crítico
        ang_critico = np.arcsin(vs1 / vs2)
        ang_critico_graus = ang_critico * 180 / np.pi
        ang_text = str(round(ang_critico_graus,1))
        self.label_33.setText('Ângulo crítico = ' + ang_text)

        # angulo, geofone e cálculo de tempo
        ngeo = self.spinBox_ngeo.value()
        if self.checkBox_solo.isChecked():
            v1 = self.solo_vs.value()
            v2 = self.spinBox_vs1.value()
            p1 = self.solo_espessura.value()
            p2 = thick
            theta_solo, a = self.geofone_to_angle(ngeo, rmin, rstep, p1)
            geo, time1 = self.reflect_travel_time(1, p1, theta_solo, v1, 0, 0, 0)
            theta = self.geofone_to_angle_2(ngeo, rmin, rstep, v1, v2, p1, p2)
            geo, time2 = self.reflect_travel_time(2, p1, 0, v1, p2, theta, v2)
            self.time_basalto = time2
            self.time_solo = time1
            self.axes_time.plot(geo, time1, color= 'brown', label='Solo')
            self.axes_time.plot(geo, time2, color= 'blue', label='Basalto')
        else:
            theta, a = self.geofone_to_angle(ngeo, rmin, rstep, thick)
            geo, time = self.reflect_travel_time(1, thick, theta, vs1, 0, 0, 0)
            self.time_basalto = time
            self.axes_time.plot(geo, time, color= 'blue', label = 'Basalto')

        self.axes_time.grid()
        self.axes_time.legend(title='Reflexão')


        #Azimutes para o calculo do coeficiente de reflexão

        A1 = -(deltaZ/ medZ) / 2
        A2 = -((deltaZ / medZ) - deltay) / 2


        Rspar_0 = A1 + 0.5*((deltavs/medvs)- deltay)*pow(np.tan(theta * np.pi / 180), 2)
        Rspar_90 = A2 + 0.5*((deltavs2/medvs2)- deltay)*pow(np.tan(theta * np.pi / 180), 2)

        self.axes.grid()
        self.axes.plot(theta, Rspar_90, '+', label='90')
        self.axes.plot(theta, Rspar_0, '+', label='0')
        self.axes2.set_xlim(self.axes.get_xlim())
        self.axes2.set_xticks(theta)
        self.axes2.set_xticklabels(a)
        self.axes2.set_xlabel('Distância (m)', size=6)
        for label in self.axes2.xaxis.get_ticklabels()[::2]:
            label.set_visible(False)

        if self.split_box0_90.isChecked():
            dif1= np.zeros(len(Rspar_90))
            for i in range(len(Rspar_90)):
                if abs(Rspar_90[i]) > abs(Rspar_0[i]):
                    dif1[i] = abs(Rspar_90[i] - Rspar_0[i]) / abs(Rspar_0[i])
                    if dif1[i] > 0.1:
                        self.axes.plot(theta[i], Rspar_0[i], 'ro')
                        self.axes.plot(theta[i], Rspar_90[i], 'ro')
                        break
                else:
                    dif1[i] = abs(Rspar_0[i] - Rspar_90[i]) / abs(Rspar_90[i])
                    if dif1[i] > 0.1:
                        self.axes.plot(theta[i], Rspar_90[i], 'ro')
                        self.axes.plot(theta[i], Rspar_0[i], 'ro')
                        break


        self.axes.legend(title='Azimute')
        self.canvas.draw()


    #Função para gerar arquivos anray para diferentes azimutes (0, 30, 45, 60, 90)
    def anray(self):
        azimute = np.array([0, 30, 45, 60, 90])
        self.anray_file(azimute)



    #Função que gera o arquivo do anray para um azimute específico.
    def anray_file(self, azimute):
        azh = azimute
        self.size = 0
        self.progressBar.setValue(self.size)
        for h in azh:
            self.size = self.size + 10
            self.progressBar.setValue(self.size)
            file = open('modelo_anray_%s.modelo' %h, 'w')
            file.write("'modelo HTI azimute %s'\n" %(h))
            file.write("/\n")
            if self.checkBox_solo.isChecked():
                file.write('%s %s %s %s\n' % (2, 4, 10, 10))
            else:
                file.write('%s %s %s %s\n' % (2, 3, 10, 10))
            #camada1
            file.write('%s %s\n' % (2, 2))
            file.write('%s %s\n' % (0, 100))
            file.write('%s %s\n' % (0, 100))
            file.write('%s %s\n' % (0, 0))
            file.write('%s %s\n' % (0, 0))
            #camada de solo
            if self.checkBox_solo.isChecked():
                file.write('%s %s\n' % (2, 2))
                file.write('%s %s\n' % (0, 100))
                file.write('%s %s\n' % (0, 100))
                file.write('%s %s\n' % (self.solo_espessura.value() / 1000, self.solo_espessura.value() / 1000))
                file.write('%s %s\n' % (self.solo_espessura.value() / 1000, self.solo_espessura.value() / 1000))

            # camada2
            file.write('%s %s\n' % (2, 2))
            file.write('%s %s\n' % (0, 100))
            file.write('%s %s\n' % (0, 100))
            file.write('%s %s\n' % (self.spinBox_thick.value()/1000, self.spinBox_thick.value()/1000))
            file.write('%s %s\n' % (self.spinBox_thick.value()/1000, self.spinBox_thick.value()/1000))
            # camada3
            file.write('%s %s\n' % (2, 2))
            file.write('%s %s\n' % (0, 100))
            file.write('%s %s\n' % (0, 100))
            file.write('%s %s\n' % (2, 2))
            file.write('%s %s\n' % (2, 2))
            #printerplot
            file.write('%s %s\n%s %s\n%s %s\n' % (0, 0.5, 0.9, 1.1, 1.9, 2.1))
            if self.checkBox_solo.isChecked():
                file.write('%s %s\n' % (1.9, 2.1))
            #especificação de parametros elásticos e densidade constante
            file.write('%s %s\n' % (0, 1))
            #densidades
            if self.checkBox_solo.isChecked():
                file.write('%s '% (self.solo_densidade.value() / 1000))
            file.write('%s %s\n' % (self.spinBox_p1.value()/1000, self.spinBox_p2.value()/1000))
            if self.checkBox_solo.isChecked():
                file.write('%s %s\n' % (0, 0))
                file.write('%s %s %s\n' % (1, 1, 1))  # homogenea em x,y,z
                file.write('/\n/\n/\n')  # gridlines
                file.write('%s\n%s\n' % ((self.solo_vp.value() / 1000) ** 2, (self.solo_vs.value() / 1000) ** 2))  # quadrado da onda P e S
            #camada isotrópica
            file.write('%s %s\n' % (0, 0))
            file.write('%s %s %s\n' % (1, 1, 1)) #homogenea em x,y,z
            file.write('/\n/\n/\n') #gridlines
            file.write('%s\n%s\n' % ((self.spinBox_vp1.value()/1000)**2, (self.spinBox_vs1.value()/1000)**2)) #quadrado da onda P e S
            # camada anisotrópica
            if self.dn and self.dt != 0:
                file.write('%s %s\n' % (1, 0))
                file.write('%s %s %s\n' % (1, 1, 1))  # homogenea em x,y,z
                file.write('/\n/\n/\n')  # gridlines
                file.write('%s\n' % (self.c11)) #A11
                file.write('%s\n' % (self.c13))  # A12
                file.write('%s\n' % (self.c13))  # A13
                file.write('%s\n' % (0))  # A14
                file.write('%s\n' % (0))  # A15
                file.write('%s\n' % (0))  # A16
                file.write('%s\n' % (self.c33))  # A22
                file.write('%s\n' % (self.c23))  # A23
                file.write('%s\n' % (0))  # A24
                file.write('%s\n' % (0))  # A25
                file.write('%s\n' % (0))  # A26
                file.write('%s\n' % (self.c33))  # A33
                file.write('%s\n' % (0))  # A34
                file.write('%s\n' % (0))  # A35
                file.write('%s\n' % (0))  # A36
                file.write('%s\n' % (self.c44))  # A44
                file.write('%s\n' % (0))  # A45
                file.write('%s\n' % (0))  # A46
                file.write('%s\n' % (self.c55))  # A55
                file.write('%s\n' % (0))  # A55
                file.write('%s\n' % (self.c55))  # A66
            else:
                file.write('%s %s\n' % (0, 0))
                file.write('%s %s %s\n' % (1, 1, 1))  # homogenea em x,y,z
                file.write('/\n/\n/\n')  # gridlines
                file.write('%s\n%s\n' % ((self.spinBox_vp2.value() / 1000) ** 2, (self.spinBox_vs2.value() / 1000) ** 2))

            #!ICONT,MEP,MOUT,MDIM,METHOD,MREG,ITMAX,IPOL,IPREC,IRAYPL,IPRINT,IAMP,MTRNS,ICOEF,IRT,ILOC,MCOD,MORI
            file.write('%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s\n' % (1, self.spinBox_ngeo.value(), 1, 1, 0, 1, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
            #!PROF(1),RMIN,RSTEP,XPRF,YPRF
            if h < 90:
                azh = (h/180)*np.pi
            else:
                azh = 1.5
            file.write('%s %s %s %s %s\n' % (azh, self.spinBox_rmin.value()/1000, self.spinBox_rstep.value()/1000,  10,  10))
            #!XSOUR,YSOUR,ZSOUR,TSOUR,DT,AC,REPS,PREPS
            file.write('%s %s %s %s %s %s %s %s\n' % (10,  10, 0, 0, 0.04, 0.0001, 0.0005, 0.0005))
            #!AMIN, ASTEP, AMAX
            file.write('%s %s %s\n' % (-0.3, 0.005, 1.8))
            #!BMIN, BSTEP, BMAX
            file.write('%s %s %s\n' % (-0.3, 0.005, 1.8))
            #!KC, KREF, ((CODE(I, K), K = 1, 2), I = 1, KREF)
            file.write('%s %s %s %s %s %s\n' % (1, 2, 1, 2, 1, 2))
            if self.checkBox_solo.isChecked():
                file.write('%s %s %s %s %s %s %s %s %s %s\n' % (1, 4, 1, 2, 2, 2, 2, 2, 1, 2))

            file.write('%s %s\n' % (0, 0))
            file.write('%s/' % (0))
            file.close()
            self.anray_script(h)

    #Função que constrói um script para rodar os modelos e gerar as figuras
    def anray_script(self, azh):
        files = open('anray_script%s.sh' %azh, 'w')
        files.write('modname=modelo_anray\nanrayinput="$modname"_%s.modelo\n./anray <<FIM\n$anrayinput\nFIM\n\n\n' %(azh))
        files.write('cp fort.30 amplitudes_%s.dat\n\n' %azh)
        files.write('cp lu2.anray lu2_%s.anray' %azh)
        files.close()
        subprocess.call('chmod +x anray_script%s.sh' %azh, shell=True)
        thread_anray = threading.Thread(target=self.anray_thr(azh))
        thread_anray.start()

    #Função para executar o script e aguardar o término da execução
    def anray_thr(self, azh):
        FNULL = open(os.devnull, 'w')
        str = './anray_script%s.sh' %azh
        p = subprocess.Popen(str, shell=True, stdout=FNULL, stderr=subprocess.STDOUT)
        status = p.wait()
        shutil.copy2('fort.30', '%s/HTI_SH_model/amplitudes_%s.dat' %(self.anray_path, azh))
        shutil.copy2('lu2.anray', '%s/HTI_SH_model/lu2_%s.anray' % (self.anray_path, azh))
        shutil.move('modelo_anray_%s.modelo' % azh,'%s/HTI_SH_model/modelo_anray_%s.modelo' % (self.anray_path, azh))
        os.remove('%s/anray_script%s.sh' %(self.anray_path, azh))

        self.size = self.size + 10
        self.progressBar.setValue(self.size)


        if self.progressBar.value() == 100:
            self.frame_7.setEnabled(True)
            self.frame_8.setEnabled(True)
            self.frame_11.setEnabled(True)
            self.frame_13.setEnabled(True)
            self.sismo_button.setEnabled(True)
            self.frame_14.setEnabled(True)
            self.frame_9.setEnabled(True)

            if self.checkBox_solo.isChecked() == False:
                self.checkBox_solo_sismo.setChecked(False)
                self.checkBox_solo_sismo2.setChecked(False)
                self.checkBox_solo_sismo.setEnabled(False)
                self.frame_12.setEnabled(False)
                self.label_47.setEnabled(False)
            else:
                self.frame_12.setEnabled(True)
                self.checkBox_solo_sismo.setEnabled(True)
                self.label_47.setEnabled(True)


            self.split()


    #Função que plota as componentes a partir do anray e analisa a separação
    def split(self):
        self.axes_anray_tot.cla()
        self.axes_anray_trans.cla()
        self.axes_anray_trans2.cla()
        self.axes_anray_tot2.cla()
        # self.axes_anray_z.cla()
        # self.axes_anray_z2.cla()
        self.axes_anray_time.cla()
        self.axes_anray2_tot.cla()
        # self.axes_anray2_z.cla()
        self.axes_anray2_time.cla()
        self.axes_anray2_rad.cla()


        f_0 = open('amplitudes_0.dat', "r")
        f_30 = open('amplitudes_30.dat', "r")
        f_45 = open('amplitudes_45.dat', "r")
        f_60 = open('amplitudes_60.dat', "r")
        f_90= open('amplitudes_90.dat', "r")
        time_basalto = []
        time_solo=[]
        geofone_0 = []
        x_0 = []
        y_0 = []
        z_0 = []
        xc_0 = []
        yc_0 = []
        zc_0 = []
        geofone_30 = []
        x_30 = []
        y_30 = []
        z_30 = []
        xc_30 = []
        yc_30 = []
        zc_30 = []
        geofone_45 = []
        x_45 = []
        y_45 = []
        z_45 = []
        xc_45 = []
        yc_45 = []
        zc_45 = []
        geofone_60 = []
        x_60 = []
        y_60 = []
        z_60 = []
        xc_60 = []
        yc_60 = []
        zc_60 = []
        geofone_90 = []
        x_90 = []
        y_90 = []
        z_90 = []
        xc_90 = []
        yc_90 = []
        zc_90 = []
        solo_x_0=[]
        solo_x_30=[]
        solo_x_45 = []
        solo_x_60 = []
        solo_x_90 = []
        solo_y_0=[]
        solo_y_30=[]
        solo_y_45 = []
        solo_y_60 = []
        solo_y_90 = []
        solo_z_0=[]
        solo_z_30=[]
        solo_z_45 = []
        solo_z_60 = []
        solo_z_90 = []
        fase_x_0 = []
        fase_x_30 = []
        fase_x_45 = []
        fase_x_60 = []
        fase_x_90 = []
        fase_y_0 = []
        fase_y_30 = []
        fase_y_45 = []
        fase_y_60 = []
        fase_y_90 = []
        fase_z_0 = []
        fase_z_30 = []
        fase_z_45 = []
        fase_z_60 = []
        fase_z_90 = []

        self.axes_anray_tot.set_ylabel("total")
        self.axes_anray_trans.set_ylabel("transversal")
        self.axes_anray_tot.grid()
        self.axes_anray_trans.grid()
        self.axes_anray_time.grid()
        self.axes_anray2_tot.set_ylabel("total")
        self.axes_anray2_rad.set_ylabel("transversal")
        self.axes_anray2_tot.grid()
        self.axes_anray2_rad.grid()
        self.axes_anray2_time.grid()

        if self.checkBox_solo.isChecked():
            two_layer = True
            var = -2
        else:
            two_layer = False
            var = -1




        for line in f_0:
            coluna = line.split()
            if float(coluna[0]) == var:
                geofone_0.append(int(coluna[1]))
                #parte real
                x_0.append(float(coluna[3]))
                y_0.append(float(coluna[5]))
                z_0.append(float(coluna[7]))
                #parte complexa
                xc_0.append(float(coluna[4]))
                yc_0.append(float(coluna[6]))
                zc_0.append(float(coluna[8]))

            if two_layer == True:
                if float(coluna[0]) == -2:
                    time_basalto.append(float(coluna[2]))
                else :
                    time_solo.append(float(coluna[2]))
                    solo_x_0.append(np.sqrt(float(coluna[3])**2+float(coluna[4])**2))
                    solo_y_0.append(np.sqrt(float(coluna[5]) ** 2 + float(coluna[6]) ** 2))
                    solo_z_0.append(np.sqrt(float(coluna[7]) ** 2 + float(coluna[8]) ** 2))
                    fase_x_0.append(np.arctan2(float(coluna[4]), float(coluna[3])))
                    fase_y_0.append(np.arctan2(float(coluna[6]), float(coluna[5])))
                    fase_z_0.append(np.arctan2(float(coluna[8]), float(coluna[7])))
            if two_layer == False:
                time_basalto.append(float(coluna[2]))



        f_0.close()
        geo_0 = np.asarray(geofone_0)
        time_basalto = np.asarray(time_basalto)
        time_solo = np.asarray(time_solo)
        x_0 = np.asarray(x_0)
        y_0 = np.asarray(y_0)
        z_0 = np.asarray(z_0)
        xc_0 = np.asarray(xc_0)
        yc_0 = np.asarray(yc_0)
        zc_0 = np.asarray(zc_0)
        solo_x_0 = np.asarray(solo_x_0)
        solo_x_0 = np.fliplr([solo_x_0])[0]
        solo_y_0 = np.asarray(solo_y_0)
        solo_y_0 = np.fliplr([solo_y_0])[0]
        solo_z_0 = np.asarray(solo_z_0)
        solo_z_0 = np.fliplr([solo_z_0])[0]
        fase_x_0 = np.asarray(fase_x_0)
        fase_x_0 = np.fliplr([fase_x_0])[0]
        fase_y_0 = np.asarray(fase_y_0)
        fase_y_0 = np.fliplr([fase_y_0])[0]
        fase_z_0 = np.asarray(fase_z_0)
        fase_z_0 = np.fliplr([fase_z_0])[0]
        solo_rad_0 = np.sqrt(solo_x_0 ** 2 + solo_y_0 ** 2)
        self.solo_fase_rad = fase_x_0
        self.solo_fase_z = fase_z_0
        solo_0_tot = np.sqrt(solo_x_0 ** 2 + solo_y_0 ** 2 + solo_z_0 ** 2)
        self.refl_solo_rad_0 = solo_rad_0
        self.refl_solo_z_0 = solo_z_0
        self.time_basalto = np.fliplr([time_basalto])[0]
        self.time_solo = np.fliplr([time_solo])[0]
        x0_re = np.fliplr([x_0])[0]
        y0_re = np.fliplr([y_0])[0]
        z0_re = np.fliplr([z_0])[0]
        x0c_re = np.fliplr([xc_0])[0]
        y0c_re = np.fliplr([yc_0])[0]
        z0c_re = np.fliplr([zc_0])[0]
        ampx_0 = np.sqrt(x0_re**2 + x0c_re**2)
        ampy_0 = np.sqrt(y0_re **2 + y0c_re ** 2)
        ampz_0 = np.sqrt(z0_re **2 + z0c_re ** 2)
        phx_0 = np.arctan2(x0c_re, x0_re)
        phy_0 = np.arctan2(y0c_re, y0_re)
        phz_0 = np.arctan2(z0c_re, z0_re)
        self.hti_fase_rad_0 = phx_0
        self.hti_fase_z_0 = phz_0
        geo0_re = np.fliplr([geo_0])[0]
        tot0 = np.sqrt(ampx_0 ** 2 + ampy_0 ** 2 + ampz_0 ** 2)
        trans_0 = np.sqrt(ampx_0 ** 2 + ampy_0 ** 2)
        self.axes_anray_tot.plot(geo0_re, tot0, label=0)
        self.refl_tot_0 = tot0
        self.refl_rad_0 = trans_0
        self.refl_z_0 = ampz_0
        # self.axes_anray_z.plot(geo0_re, ampz_0, label=0)
        self.axes_anray_trans.plot(geo0_re, trans_0, label=0)

        if two_layer==True:
            self.axes_anray2_tot.plot(geo0_re, solo_0_tot, label=0)
            self.axes_anray2_tot.set_ylim([0,2])
            self.axes_anray2_rad.plot(geo0_re, solo_rad_0, label=0)
            # self.axes_anray2_z.plot(geo0_re, solo_z_0, label=0)



        if two_layer == True:
            self.axes_anray_time.plot(geo0_re, self.time_basalto, color='blue')
            self.axes_anray2_time.plot(geo0_re, self.time_solo, color='brown')
        else:
            self.axes_anray_time.plot(geo0_re, self.time_basalto, color='blue')

        self.axes_anray_time.set_ylabel('tempo (s)')
        self.axes_anray2_time.set_ylabel('tempo (s)')


        for line in f_30:
            coluna = line.split()
            if float(coluna[0]) == var:
                geofone_30.append(int(coluna[1]))
                x_30.append(float(coluna[3]))
                y_30.append(float(coluna[5]))
                z_30.append(float(coluna[7]))
                xc_30.append(float(coluna[4]))
                yc_30.append(float(coluna[6]))
                zc_30.append(float(coluna[8]))

            if two_layer == True:
                if float(coluna[0]) == -1:
                    solo_x_30.append(np.sqrt(float(coluna[3])**2+float(coluna[4])**2))
                    solo_y_30.append(np.sqrt(float(coluna[5]) ** 2 + float(coluna[6]) ** 2))
                    solo_z_30.append(np.sqrt(float(coluna[7]) ** 2 + float(coluna[8]) ** 2))
                    fase_x_30.append(np.arctan2(float(coluna[4]), float(coluna[3])))
                    fase_y_30.append(np.arctan2(float(coluna[6]), float(coluna[5])))
                    fase_z_30.append(np.arctan2(float(coluna[8]), float(coluna[7])))



        f_30.close()
        geo_30 = np.asarray(geofone_30)
        x_30 = np.asarray(x_30)
        y_30 = np.asarray(y_30)
        z_30 = np.asarray(z_30)
        xc_30 = np.asarray(xc_30)
        yc_30 = np.asarray(yc_30)
        zc_30 = np.asarray(zc_30)
        x30_re = np.fliplr([x_30])[0]
        y30_re = np.fliplr([y_30])[0]
        z30_re = np.fliplr([z_30])[0]
        x30c_re = np.fliplr([xc_30])[0]
        y30c_re = np.fliplr([yc_30])[0]
        z30c_re = np.fliplr([zc_30])[0]
        ampx_30 = np.sqrt(x30_re ** 2 + x30c_re ** 2)
        ampy_30 = np.sqrt(y30_re ** 2 + y30c_re ** 2)
        ampz_30 = np.sqrt(z30_re ** 2 + z30c_re ** 2)
        phx_30 = np.arctan2(x30c_re, x30_re)
        phy_30 = np.arctan2(y30c_re, y30_re)
        phz_30 = np.arctan2(z30c_re, z30_re)
        self.hti_fase_rad_30 = phx_30
        self.hti_fase_z_30 = phz_30
        geo30_re = np.fliplr([geo_30])[0]
        tot30 = np.sqrt(ampx_30 ** 2 + ampy_30 ** 2 + ampz_30 ** 2)
        trans_30 = np.sqrt(ampx_30 ** 2 + ampy_30 ** 2)
        solo_x_30 = np.asarray(solo_x_30)
        solo_x_30 = np.fliplr([solo_x_30])[0]
        solo_y_30 = np.asarray(solo_y_30)
        solo_y_30 = np.fliplr([solo_y_30])[0]
        solo_z_30 = np.asarray(solo_z_30)
        solo_z_30 = np.fliplr([solo_z_30])[0]
        solo_30_tot = np.sqrt(solo_x_30 ** 2 + solo_y_30 ** 2 + solo_z_30 ** 2)
        solo_rad_30 = np.sqrt(solo_x_30 ** 2 + solo_y_30 ** 2)
        fase_x_30 = np.asarray(fase_x_30)
        fase_x_30 = np.fliplr([fase_x_30])[0]
        fase_y_30 = np.asarray(fase_y_30)
        fase_y_30 = np.fliplr([fase_y_30])[0]
        fase_z_30 = np.asarray(fase_z_30)
        fase_z_30 = np.fliplr([fase_z_30])[0]
        self.refl_solo_x_30 = solo_rad_30
        self.refl_solo_y_30 = solo_y_30
        self.refl_solo_z_30 = solo_z_30
        self.refl_tot_30 = tot30
        self.refl_rad_30 = trans_30
        self.refl_y_30 = y30_re
        self.refl_z_30 = ampz_30
        self.axes_anray_tot.plot(geo30_re, tot30, label=30)
        self.axes_anray_trans.plot(geo30_re, trans_30, label=30)
        if two_layer == True:
            self.axes_anray2_tot.plot(geo30_re, solo_30_tot, label=30)
            self.axes_anray2_rad.plot(geo30_re, solo_rad_30, label=30)


        for line in f_45:
            coluna = line.split()
            if float(coluna[0]) == var:
                geofone_45.append(int(coluna[1]))
                x_45.append(float(coluna[3]))
                y_45.append(float(coluna[5]))
                z_45.append(float(coluna[7]))
                xc_45.append(float(coluna[4]))
                yc_45.append(float(coluna[6]))
                zc_45.append(float(coluna[8]))

            if two_layer == True:
                if float(coluna[0]) == -1:
                    solo_x_45.append(np.sqrt(float(coluna[3])**2+float(coluna[4])**2))
                    solo_y_45.append(np.sqrt(float(coluna[5]) ** 2 + float(coluna[6]) ** 2))
                    solo_z_45.append(np.sqrt(float(coluna[7]) ** 2 + float(coluna[8]) ** 2))
                    fase_x_45.append(np.arctan2(float(coluna[4]), float(coluna[3])))
                    fase_y_45.append(np.arctan2(float(coluna[6]), float(coluna[5])))
                    fase_z_45.append(np.arctan2(float(coluna[8]), float(coluna[7])))

        f_45.close()
        geo_45 = np.asarray(geofone_45)
        x_45 = np.asarray(x_45)
        y_45 = np.asarray(y_45)
        z_45 = np.asarray(z_45)
        xc_45 = np.asarray(xc_45)
        yc_45 = np.asarray(yc_45)
        zc_45 = np.asarray(zc_45)
        x45_re = np.fliplr([x_45])[0]
        y45_re = np.fliplr([y_45])[0]
        z45_re = np.fliplr([z_45])[0]
        x45c_re = np.fliplr([xc_45])[0]
        y45c_re = np.fliplr([yc_45])[0]
        z45c_re = np.fliplr([zc_45])[0]
        ampx_45 = np.sqrt(x45_re ** 2 + x45c_re ** 2)
        ampy_45 = np.sqrt(y45_re ** 2 + y45c_re ** 2)
        ampz_45 = np.sqrt(z45_re ** 2 + z45c_re ** 2)
        phx_45 = np.arctan2(x45c_re, x45_re)
        phy_45 = np.arctan2(y45c_re, y45_re)
        phz_45 = np.arctan2(z45c_re, z45_re)
        self.hti_fase_rad_45 = phx_45
        self.hti_fase_z_45 = phz_45
        geo45_re = np.fliplr([geo_45])[0]
        tot45 = np.sqrt(ampx_45 ** 2 + ampy_45 ** 2 + ampz_45 ** 2)
        trans_45 = np.sqrt(ampx_45 ** 2 + ampy_45 ** 2)
        solo_x_45 = np.asarray(solo_x_45)
        solo_x_45 = np.fliplr([solo_x_45])[0]
        solo_y_45 = np.asarray(solo_y_45)
        solo_y_45 = np.fliplr([solo_y_45])[0]
        solo_z_45 = np.asarray(solo_z_45)
        solo_z_45 = np.fliplr([solo_z_45])[0]
        solo_45_tot = np.sqrt(solo_x_45 ** 2 + solo_y_45 ** 2 + solo_z_45 ** 2)
        solo_rad_45 = np.sqrt(solo_x_45 ** 2 + solo_y_45 ** 2)
        fase_x_45 = np.asarray(fase_x_45)
        fase_x_45 = np.fliplr([fase_x_45])[0]
        fase_y_45 = np.asarray(fase_y_45)
        fase_y_45 = np.fliplr([fase_y_45])[0]
        fase_z_45 = np.asarray(fase_z_45)
        fase_z_45 = np.fliplr([fase_z_45])[0]
        self.refl_solo_x_45 = solo_rad_45
        self.refl_solo_y_45 = solo_y_45
        self.refl_solo_z_45 = solo_z_45
        self.refl_tot_45 = tot45
        self.refl_rad_45 = trans_45
        self.refl_y_45 = y45_re
        self.refl_z_45 = ampz_45
        self.axes_anray_tot.plot(geo45_re, tot45, label=45)
        self.axes_anray_trans.plot(geo45_re, trans_45, label=45)
        if two_layer == True:
            self.axes_anray2_tot.plot(geo45_re, solo_45_tot, label=45)
            self.axes_anray2_rad.plot(geo45_re, solo_rad_45, label=45)



        for line in f_60:
            coluna = line.split()
            if float(coluna[0]) == var:
                geofone_60.append(int(coluna[1]))
                x_60.append(float(coluna[3]))
                y_60.append(float(coluna[5]))
                z_60.append(float(coluna[7]))
                xc_60.append(float(coluna[4]))
                yc_60.append(float(coluna[6]))
                zc_60.append(float(coluna[8]))

            if two_layer == True:
                if float(coluna[0]) == -1:
                    solo_x_60.append(np.sqrt(float(coluna[3])**2+float(coluna[4])**2))
                    solo_y_60.append(np.sqrt(float(coluna[5]) ** 2 + float(coluna[6]) ** 2))
                    solo_z_60.append(np.sqrt(float(coluna[7]) ** 2 + float(coluna[8]) ** 2))
                    fase_x_60.append(np.arctan2(float(coluna[4]), float(coluna[3])))
                    fase_y_60.append(np.arctan2(float(coluna[6]), float(coluna[5])))
                    fase_z_60.append(np.arctan2(float(coluna[8]), float(coluna[7])))



        f_60.close()
        geo_60 = np.asarray(geofone_60)
        x_60 = np.asarray(x_60)
        y_60 = np.asarray(y_60)
        z_60 = np.asarray(z_60)
        xc_60 = np.asarray(xc_60)
        yc_60 = np.asarray(yc_60)
        zc_60 = np.asarray(zc_60)
        x60_re = np.fliplr([x_60])[0]
        y60_re = np.fliplr([y_60])[0]
        z60_re = np.fliplr([z_60])[0]
        x60c_re = np.fliplr([xc_60])[0]
        y60c_re = np.fliplr([yc_60])[0]
        z60c_re = np.fliplr([zc_60])[0]
        ampx_60 = np.sqrt(x60_re ** 2 + x60c_re ** 2)
        ampy_60 = np.sqrt(y60_re ** 2 + y60c_re ** 2)
        ampz_60 = np.sqrt(z60_re ** 2 + z60c_re ** 2)
        phx_60 = np.arctan2(x60c_re, x60_re)
        phy_60 = np.arctan2(y60c_re, y60_re)
        phz_60 = np.arctan2(z60c_re, z60_re)
        self.hti_fase_rad_60 = phx_60
        self.hti_fase_z_60 = phz_60
        geo60_re = np.fliplr([geo_60])[0]
        tot60 = np.sqrt(ampx_60 ** 2 + ampy_60 ** 2 + ampz_60 ** 2)
        trans_60 = np.sqrt(ampx_60 ** 2 + ampy_60 ** 2)
        solo_x_60 = np.asarray(solo_x_60)
        solo_x_60 = np.fliplr([solo_x_60])[0]
        solo_y_60 = np.asarray(solo_y_60)
        solo_y_60 = np.fliplr([solo_y_60])[0]
        solo_z_60 = np.asarray(solo_z_60)
        solo_z_60 = np.fliplr([solo_z_60])[0]
        solo_60_tot = np.sqrt(solo_x_60 ** 2 + solo_y_60 ** 2 + solo_z_60 ** 2)
        solo_rad_60 = np.sqrt(solo_x_60 ** 2 + solo_y_60 ** 2)
        fase_x_60 = np.asarray(fase_x_60)
        fase_x_60 = np.fliplr([fase_x_60])[0]
        fase_y_60 = np.asarray(fase_y_60)
        fase_y_60 = np.fliplr([fase_y_60])[0]
        fase_z_60 = np.asarray(fase_z_60)
        fase_z_60 = np.fliplr([fase_z_60])[0]
        self.refl_solo_x_60 = solo_rad_60
        self.refl_solo_y_60 = solo_y_60
        self.refl_solo_z_60 = solo_z_60
        self.refl_tot_60 = tot60
        self.refl_rad_60 = trans_60
        self.refl_y_60 = y60_re
        self.refl_z_60 = ampz_60
        self.axes_anray_tot.plot(geo60_re, tot60, label=60)
        self.axes_anray_trans.plot(geo60_re, trans_60, label=60)
        if two_layer == True:
            self.axes_anray2_tot.plot(geo60_re, solo_60_tot, label=60)
            self.axes_anray2_rad.plot(geo60_re, solo_rad_60, label=60)


        for line in f_90:
            coluna = line.split()
            if float(coluna[0]) == var:
                geofone_90.append(int(coluna[1]))
                x_90.append(float(coluna[3]))
                y_90.append(float(coluna[5]))
                z_90.append(float(coluna[7]))
                xc_90.append(float(coluna[4]))
                yc_90.append(float(coluna[6]))
                zc_90.append(float(coluna[8]))

            if two_layer == True:
                if float(coluna[0]) == -1:
                    solo_x_90.append(np.sqrt(float(coluna[3])**2+float(coluna[4])**2))
                    solo_y_90.append(np.sqrt(float(coluna[5]) ** 2 + float(coluna[6]) ** 2))
                    solo_z_90.append(np.sqrt(float(coluna[7]) ** 2 + float(coluna[8]) ** 2))
                    fase_x_90.append(np.arctan2(float(coluna[4]), float(coluna[3])))
                    fase_y_90.append(np.arctan2(float(coluna[6]), float(coluna[5])))
                    fase_z_90.append(np.arctan2(float(coluna[8]), float(coluna[7])))

        f_90.close()
        geo_90 = np.asarray(geofone_90)
        x_90 = np.asarray(x_90)
        y_90 = np.asarray(y_90)
        z_90 = np.asarray(z_90)
        xc_90 = np.asarray(xc_90)
        yc_90 = np.asarray(yc_90)
        zc_90 = np.asarray(zc_90)
        x90_re = np.fliplr([x_90])[0]
        y90_re = np.fliplr([y_90])[0]
        z90_re = np.fliplr([z_90])[0]
        x90c_re = np.fliplr([xc_90])[0]
        y90c_re = np.fliplr([yc_90])[0]
        z90c_re = np.fliplr([zc_90])[0]
        ampx_90 = np.sqrt(x90_re ** 2 + x90c_re ** 2)
        ampy_90 = np.sqrt(y90_re ** 2 + y90c_re ** 2)
        ampz_90 = np.sqrt(z90_re ** 2 + z90c_re ** 2)
        phx_90 = np.arctan2(x90c_re, x90_re)
        phy_90 = np.arctan2(y90c_re, y90_re)
        phz_90 = np.arctan2(z90c_re, z90_re)
        self.hti_fase_rad_90 = phx_90
        self.hti_fase_z_90 = phz_90
        geo90_re = np.fliplr([geo_90])[0]
        tot90 = np.sqrt(ampx_90 ** 2 + ampy_90 ** 2 + ampz_90 ** 2)
        trans_90 = np.sqrt(ampx_90 ** 2 + ampy_90 ** 2)
        solo_x_90 = np.asarray(solo_x_90)
        solo_x_90 = np.fliplr([solo_x_90])[0]
        solo_y_90 = np.asarray(solo_y_90)
        solo_y_90 = np.fliplr([solo_y_90])[0]
        solo_z_90 = np.asarray(solo_z_90)
        solo_z_90 = np.fliplr([solo_z_90])[0]
        solo_90_tot = np.sqrt(solo_x_90 ** 2 + solo_y_90 ** 2 + solo_z_90 ** 2)
        solo_rad_90 = np.sqrt(solo_x_90 ** 2 + solo_y_90 ** 2)
        fase_x_90 = np.asarray(fase_x_90)
        fase_x_90 = np.fliplr([fase_x_90])[0]
        fase_y_90 = np.asarray(fase_y_90)
        fase_y_90 = np.fliplr([fase_y_90])[0]
        fase_z_90 = np.asarray(fase_z_90)
        fase_z_90 = np.fliplr([fase_z_90])[0]
        self.refl_solo_x_90 = solo_rad_90
        self.refl_solo_y_90 = solo_y_90
        self.refl_solo_z_90 = solo_z_90
        self.refl_tot_90 = tot90
        self.refl_rad_90 = trans_90
        self.refl_y_90 = y90_re
        self.refl_z_90 = ampz_90
        self.axes_anray_tot.plot(geo90_re, tot90, label=90)
        self.axes_anray_trans.plot(geo90_re, trans_90, label=90)
        self.axes_anray_tot.legend(title='Azimute', fontsize=6, loc=2, ncol=5, bbox_to_anchor=(0, 1.5))
        if two_layer == True:
            self.axes_anray2_tot.plot(geo90_re, solo_90_tot, label=90)
            self.axes_anray2_rad.plot(geo90_re, solo_rad_90, label=90)
            self.axes_anray2_tot.legend(title='Azimute', fontsize=6, loc=2, ncol=5, bbox_to_anchor=(0, 1.4))



        if len(trans_0)>len(trans_90):
            s = len(trans_0)- len(trans_90)
            print(s)
            for i in range(s):
                print(i)
                trans_0 = np.delete(trans_0, 0)
                ampz_0 = np.delete(ampz_0, 0)
                geo0_re = np.delete(geo0_re, 0)

        if self.split_box_anray_0_90.isChecked():
            split_trans = np.zeros(len(geo0_re))
            split_rad = np.zeros(len(geo0_re))
            split_z = np.zeros(len(geo0_re))

            for i in range(len(geo0_re)):
                if trans_0[i] > trans_90[i]:
                    split_trans[i] = (trans_0[i]-trans_90[i])/trans_90[i]
                    if split_trans[i] > 0.1:
                        self.axes_anray_trans.plot(geo0_re[i], trans_0[i], 'r+')
                        self.axes_anray_trans.plot(geo0_re[i], trans_90[i], 'r+')
                else:
                    split_trans[i] = (trans_90[i] - trans_0[i]) / trans_0[i]
                    if split_trans[i] > 0.1:
                        self.axes_anray_trans.plot(geo0_re[i], trans_0[i], 'r+')
                        self.axes_anray_trans.plot(geo0_re[i], trans_90[i], 'r+')




            self.axes_anray_trans2.bar(geo0_re, split_trans, width=0.7, alpha=0.1, color='red')


        if self.split_box_anray_0_45.isChecked():
            split_trans = np.zeros(len(geo45_re))
            split_rad = np.zeros(len(geo45_re))
            split_z = np.zeros(len(geo45_re))

            for i in range(len(geo45_re)):
                if trans_0[i] > trans_45[i]:
                    split_trans[i] = (trans_0[i] - trans_45[i]) / trans_45[i]
                    if split_trans[i] > 0.1:
                        self.axes_anray_trans.plot(geo45_re[i], trans_0[i], 'b+')
                        self.axes_anray_trans.plot(geo45_re[i], trans_45[i], 'b+')
                else:
                    split_trans[i] = (trans_45[i] - trans_0[i]) / trans_0[i]
                    if split_trans[i] > 0.1:
                        self.axes_anray_trans.plot(geo45_re[i], trans_0[i], 'b+')
                        self.axes_anray_trans.plot(geo45_re[i], trans_45[i], 'b+')



            self.axes_anray_trans2.bar(geo45_re, split_trans, width=0.7, alpha=0.1, color='blue')


        if self.split_box_anray_30_60.isChecked():

            split_trans = np.zeros(len(geo30_re))
            split_rad = np.zeros(len(geo30_re))
            split_z = np.zeros(len(geo30_re))

            for i in range(len(geo30_re)):
                if trans_30[i] > trans_60[i]:
                    split_trans[i] = (trans_30[i] - trans_60[i]) / trans_60[i]
                    if split_trans[i] > 0.1:
                        self.axes_anray_tot.plot(geo30_re[i], trans_30[i], 'g+')
                        self.axes_anray_tot.plot(geo30_re[i], trans_60[i], 'g+')
                else:
                    split_trans[i] = (trans_60[i] - trans_30[i]) / trans_30[i]
                    if split_trans[i] > 0.1:
                        self.axes_anray_trans.plot(geo30_re[i], trans_30[i], 'g+')
                        self.axes_anray_trans.plot(geo30_re[i], trans_60[i], 'g+')


            self.axes_anray_trans2.bar(geo30_re, split_trans, width=0.7, alpha=0.1, color='green')


        if self.split_box_anray_45_90.isChecked():

            split_trans = np.zeros(len(geo45_re))
            split_rad = np.zeros(len(geo45_re))
            split_z = np.zeros(len(geo45_re))

            for i in range(len(geo45_re)):
                if trans_45[i] > trans_90[i]:
                    split_trans[i] = (trans_45[i] - trans_90[i]) / trans_90[i]
                    if split_trans[i] > 0.1:
                        self.axes_anray_trans.plot(geo45_re[i], trans_45[i], 'y+')
                        self.axes_anray_trans.plot(geo45_re[i], trans_90[i], 'y+')
                else:
                    split_trans[i] = (trans_90[i] - trans_45[i]) / trans_45[i]
                    if split_trans[i] > 0.1:
                        self.axes_anray_trans.plot(geo45_re[i], trans_45[i], 'y+')
                        self.axes_anray_trans.plot(geo45_re[i], trans_90[i], 'y+')



                self.axes_anray_trans2.bar(geo45_re, split_trans, width=0.7, alpha=0.05, color='yellow')


        self.canvas_anray.draw()
        self.canvas_anray2.draw()

        self.plot_sismograma()


    #Função para atualizar e mostrar os coeficientes de reflexão quando selecionado o azimute, xv é a componente radial e zv, vertical
    def plot_sismograma_v(self):

        if self.radioButton_0.isChecked():
            xv = self.refl_rad_0
            zv = self.refl_z_0

        if self.radioButton_30.isChecked():
            xv = self.refl_rad_30
            zv = self.refl_z_30

        if self.radioButton_45.isChecked():
            xv = self.refl_rad_45
            zv = self.refl_z_45

        if self.radioButton_60.isChecked():
            xv = self.refl_rad_60
            zv = self.refl_z_60

        if self.radioButton_90.isChecked():
            xv = self.refl_rad_90
            zv = self.refl_z_90

        self.label_x_max.setText(str((round(np.max(abs(xv)), 4))))






    #Função para plotar o sismograma
    def plot_sismograma(self):
        self.axes_sismo_x.cla()
        Tmax = self.doubleSpinBox_tmax.value()
        dt = self.doubleSpinBox_dt.value()
        NS = int((Tmax / dt) + 1)

        t = np.arange(NS) * dt

        t1 = self.time_basalto


        x1 = self.spinBox_rmin.value()
        dx = self.spinBox_rstep.value()
        NX = self.spinBox_ngeo.value()
        x = np.arange(NX) * dx + x1
        normal_f = self.doubleSpinBox_normalf.value()
        dados_x = np.zeros([NX, NS])
        dados_z = np.zeros([NX, NS])

        FREQ = self.doubleSpinBox_freq.value()
        OMEGA = 2 * np.pi * FREQ
        GAMA = self.doubleSpinBox_gama.value()
        PSI = 0.
        TSH = 0.45 * GAMA / FREQ
        tw = np.arange(-TSH, TSH + dt, dt)
        wr_hti = []
        wz_hti = []
        wr_solo = []
        wz_solo = []

        for i in range(0, NX):
            ni = int(t1[i] / dt)
            dados_x[i, ni] = 1
            dados_z[i, ni] = 1

        if self.checkBox_solo.isChecked():
            self.frame_12.setEnabled(True)
            self.checkBox_solo_sismo.setEnabled(True)
            self.label_47.setEnabled(True)
            t2 = self.time_solo
            dados_solo_x = np.zeros([NX, NS])
            dados_solo_z = np.zeros([NX, NS])
            for i in range(0, NX):
                wr = np.cos(OMEGA * tw + self.solo_fase_rad[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                wz = np.cos(OMEGA * tw + self.solo_fase_z[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                wr_solo.append(wr)
                wz_solo.append(wz)

                ni2 = int(t2[i] / dt)
                dados_solo_x[i, ni2] = 1
                dados_solo_z[i, ni2] = 1


        if self.radioButton_0.isChecked():
            xv = self.refl_rad_0
            zv = self.refl_z_0
            for i in range(0, NX):
                wr = np.cos(OMEGA * tw + self.hti_fase_rad_0[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                wz = np.cos(OMEGA * tw + self.hti_fase_z_0[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                wr_hti.append(wr)
                wz_hti.append(wz)
            if self.checkBox_solo_sismo.isChecked():
                xv2 = self.refl_solo_rad_0
                zv2 = self.refl_solo_z_0


        if self.radioButton_30.isChecked():
            xv = self.refl_rad_30
            zv = self.refl_z_30
            for i in range(0, NX):
                wr = np.cos(OMEGA * tw + self.hti_fase_rad_30[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                wz = np.cos(OMEGA * tw + self.hti_fase_z_30[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                wr_hti.append(wr)
                wz_hti.append(wz)
            if self.checkBox_solo_sismo.isChecked():
                xv2 = self.refl_solo_x_30
                zv2 = self.refl_solo_z_30

        if self.radioButton_45.isChecked():
            xv = self.refl_rad_45
            zv = self.refl_z_45
            for i in range(0, NX):
                wr = np.cos(OMEGA * tw + self.hti_fase_rad_45[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                wz = np.cos(OMEGA * tw + self.hti_fase_z_45[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                wr_hti.append(wr)
                wz_hti.append(wz)
            if self.checkBox_solo_sismo.isChecked():
                xv2 = self.refl_solo_x_45
                zv2 = self.refl_solo_z_45

        if self.radioButton_60.isChecked():
            xv = self.refl_rad_60
            zv = self.refl_z_60
            for i in range(0, NX):
                wr = np.cos(OMEGA * tw + self.hti_fase_rad_60[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                wz = np.cos(OMEGA * tw + self.hti_fase_z_60[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                wr_hti.append(wr)
                wz_hti.append(wz)
            if self.checkBox_solo_sismo.isChecked():
                xv2 = self.refl_solo_x_60
                zv2 = self.refl_solo_z_60

        if self.radioButton_90.isChecked():
            xv = self.refl_rad_90
            zv = self.refl_z_90
            for i in range(0, NX):
                wr = np.cos(OMEGA * tw + self.hti_fase_rad_90[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                wz = np.cos(OMEGA * tw + self.hti_fase_z_90[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                wr_hti.append(wr)
                wz_hti.append(wz)
            if self.checkBox_solo_sismo.isChecked():
                xv2 = self.refl_solo_x_90
                zv2 = self.refl_solo_z_90



        self.plot_sismograma_v()


        fatorganhodisplay = normal_f

        if self.radioButton_normx.isChecked():
            fatorganhodisplay = 1/np.max(abs(xv))


        if self.radioButton_norm_def.isChecked():
            fatorganhodisplay = 1/self.doubleSpinBox_normalf.value()



        for i in range(dados_x.shape[0]):
            wx = wr_hti[i]*xv[i]
            wz = wz_hti[i]*zv[i]
            dados_x[i, :] = np.convolve(dados_x[i, :], wx, mode='same')
            dados_z[i, :] = np.convolve(dados_z[i, :], wz, mode='same')

        if self.checkBox_solo_sismo.isChecked():
            self.checkBox_solo_sismo2.setEnabled(True)

            for i in range(dados_x.shape[0]):
                if self.checkBox_solo_sismo2.isChecked():
                    if i == 0:
                        wx2 = wr_solo[i] * xv2[i]
                        wz2 = wz_solo[i] * zv2[i]
                        dados_solo_x[i, :] = np.convolve(dados_solo_x[i, :], wx2, mode='same')
                        dados_solo_z[i, :] = np.convolve(dados_solo_z[i, :], wz2, mode='same')
                    else:
                        dados_solo_x[i, :] = 0
                        dados_solo_z[i, :] = 0
                else:
                    wx2 = wr_solo[i] * xv2[i]
                    wz2 = wz_solo[i] * zv2[i]
                    dados_solo_x[i, :] = np.convolve(dados_solo_x[i, :], wx2, mode='same')
                    dados_solo_z[i, :] = np.convolve(dados_solo_z[i, :], wz2, mode='same')



            for i in range(0, NX):
                data_x = x[i]+ (dados_x[i] + dados_solo_x[i]) * fatorganhodisplay
                data_z = x[i] +(dados_z[i] + dados_solo_z[i]) * fatorganhodisplay
                self.axes_sismo_x.plot(data_x, t, '-', color='black')
                self.axes_sismo_x.fill_betweenx(t, x[i], data_x, where=(data_x > x[i]), color='black')
                self.axes_sismo_x.set_ylim([np.max(t), self.doubleSpinBox_tmin.value()])

        else:
            for i in range(0, NX):
                data_x = x[i] + dados_x[i] * fatorganhodisplay
                data_z = x[i] + dados_z[i] * fatorganhodisplay
                self.axes_sismo_x.plot(data_x, t, '-', color='black')
                self.axes_sismo_x.fill_betweenx(t, x[i], data_x , where=(data_x > x[i]), color='black')
                self.axes_sismo_x.set_ylim([np.max(t), self.doubleSpinBox_tmin.value()])



        self.canvas_sismo.draw()
        self.plot_sismo_azim()
        self.az_tmax.setValue(np.max(t))


    #Plota os sismogramas da mesma componente para azimutes diferentes. Normalizado de forma ao maior valor entre os dois ser igual a 1.
    def plot_sismo_azim(self):

        self.axes_sismo2_1.cla()
        self.axes_sismo2_2.cla()
        Tmax = self.doubleSpinBox_tmax.value()
        dt = self.doubleSpinBox_dt.value()
        NS = int((Tmax / dt) + 1)
        t = np.arange(NS) * dt
        t1 = self.time_basalto
        x1 = self.spinBox_rmin.value()
        dx = self.spinBox_rstep.value()
        NX = self.spinBox_ngeo.value()
        x = np.arange(NX) * dx + x1
        dados_1 = np.zeros([NX, NS])
        dados_2 = np.zeros([NX, NS])
        w_1=[]
        w_2=[]

        r1 = 0
        r2 = 0

        try:

            for i in range(0, NX):
                ni = int(t1[i] / dt)
                dados_1[i, ni] = 1
                dados_2[i, ni] = 1


            FREQ = 50
            OMEGA = 2 * np.pi * FREQ
            GAMA = 4.
            PSI = 0.
            TSH = 0.45 * GAMA / FREQ
            tw = np.arange(-TSH, TSH + dt, dt)
            w = np.cos(OMEGA * tw + PSI) * np.exp(-(OMEGA * tw / GAMA) ** 2)

            if self.radio_sismo_0_90.isChecked():
                label1 = '0'
                label2 = '90'
                if self.radioButton_plot_x.isChecked():
                    r1 = self.refl_rad_0
                    r2 = self.refl_rad_90
                    max_1 = np.max(abs(self.refl_rad_0))
                    max_2 = np.max(abs(self.refl_rad_90))
                    for i in range(0, NX):
                        w1 = np.cos(OMEGA * tw + self.hti_fase_rad_0[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                        w2 = np.cos(OMEGA * tw + self.hti_fase_rad_90[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                        w_1.append(w1)
                        w_2.append(w2)


            if self.radio_sismo_0_45.isChecked():
                label1 = '0'
                label2 = '45'
                if self.radioButton_plot_x.isChecked():
                    r1 = self.refl_rad_0
                    r2 = self.refl_rad_45
                    max_1 = np.max(abs(self.refl_rad_0))
                    max_2 = np.max(abs(self.refl_rad_45))
                    for i in range(0, NX):
                        w1 = np.cos(OMEGA * tw + self.hti_fase_rad_0[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                        w2 = np.cos(OMEGA * tw + self.hti_fase_rad_45[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                        w_1.append(w1)
                        w_2.append(w2)



            if self.radio_sismo_30_60.isChecked():
                label1 = '30'
                label2 = '60'
                if self.radioButton_plot_x.isChecked():
                    r1 = self.refl_rad_30
                    r2 = self.refl_rad_60
                    max_1 = np.max(abs(self.refl_rad_30))
                    max_2 = np.max(abs(self.refl_rad_60))
                    for i in range(0, NX):
                        w1 = np.cos(OMEGA * tw + self.hti_fase_rad_30[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                        w2 = np.cos(OMEGA * tw + self.hti_fase_rad_60[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                        w_1.append(w1)
                        w_2.append(w2)



            if self.radio_sismo_45_90.isChecked():
                label1 = '45'
                label2 = '90'
                if self.radioButton_plot_x.isChecked():
                    r1 = self.refl_rad_45
                    r2 = self.refl_rad_90
                    max_1 = np.max(abs(self.refl_rad_45))
                    max_2 = np.max(abs(self.refl_rad_90))
                    for i in range(0, NX):
                        w1 = np.cos(OMEGA * tw + self.hti_fase_rad_45[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                        w2 = np.cos(OMEGA * tw + self.hti_fase_rad_90[i]) * np.exp(-(OMEGA * tw / GAMA) ** 2)
                        w_1.append(w1)
                        w_2.append(w2)





            for i in range(dados_1.shape[0]):
                w1 = w_1[i] * r1[i]
                w2 = w_2[i] * r2[i]

                dados_1[i, :] = np.convolve(dados_1[i, :], w1, mode='same')
                dados_2[i, :] = np.convolve(dados_2[i, :], w2, mode='same')



            if max_1 > max_2:
                fatorganhodisplay = 1/max_1

            else:
                fatorganhodisplay = 1/max_2



            for i in range(0, NX):
                data_1 = x[i] + dados_1[i] * fatorganhodisplay
                data_2 = x[i] + dados_2[i] * fatorganhodisplay
                self.axes_sismo2_1.plot(data_1, t, '-', color='black')
                self.axes_sismo2_1.set_title('azimute %s' %label1)
                self.axes_sismo2_1.set_ylabel('Tempo (s)')
                self.axes_sismo2_1.set_xlabel('Distância (m)')
                self.axes_sismo2_1.fill_betweenx(t, x[i], data_1 , where=(data_1 > x[i]), color='black')
                self.axes_sismo2_2.plot(data_2, t, '-', color='black')
                self.axes_sismo2_2.set_title('azimute %s' %label2)
                self.axes_sismo2_2.set_ylabel('Tempo (s)')
                self.axes_sismo2_2.set_xlabel('Distância (m)')
                self.axes_sismo2_2.fill_betweenx(t, x[i], data_2, where=(data_2 > x[i]), color='black')
                self.axes_sismo2_1.set_ylim([self.az_tmax.value(), self.az_tmin.value()])
                self.axes_sismo2_2.set_ylim([self.az_tmax.value(), self.az_tmin.value()])

            self.canvas_sismo2.draw()
        except:
            self.message()

    def sismo_enable(self):
        if self.checkBox_solo_sismo.isChecked():
            self.checkBox_solo_sismo2.setEnabled(True)
        else:
            self.checkBox_solo_sismo2.setEnabled(False)





    # Função que converte a distancia dos geofones para ângulo (1 camada)
    def geofone_to_angle(self, number, rmin, rstep, prof):

        a = []

        for i in range(number):
            if i == 0:
                a.append(rmin)
                dist = rmin
            else:
                dist = dist + rstep
                a.append(dist)

        array = np.asarray(a)

        angles = np.degrees(np.arctan((array / 2) / prof))
        return angles, a

    # Função que converte a distancia dos geofones para ângulo (2 camadas),
    # v1 = velocidade no solo, v2=  velocidade na camada 1
    # p1=espessura do solo, p2=espessura da 1 camada
    def geofone_to_angle_2(self, number, rmin, rstep, v1, v2, p1, p2):
        li = []

        for i in range(number):
            if i == 0:
                li.append(rmin)
                dist = rmin
            else:
                dist = dist + rstep
                li.append(dist)

        arr = np.asarray(li)


        a = v1 ** 2 - v2 ** 2
        z = arr / 2
        b = 2 * z * a
        c = a * (z ** 2) - (v2 ** 2) * (p2 ** 2) + (v1 ** 2) * (p1 ** 2)
        d = 2 * z * ((v2 ** 2) * (p2 ** 2))
        e = (v1 ** 2) * (p1 ** 2) - (v2 ** 2) * (p2 ** 2) * (z ** 2)
        p = [a, -b, c, d, e]

        j = []

        for i in range(len(li)):
            vlist = list()
            v = roots(a * x ** 4 - b[i] * x ** 3 + c[i] * x ** 2 + d[i] * x + e[i], x)
            for po in v.keys():
                if "I" not in str(po) and po > 0 and po < arr[i]:
                    vlist.append(po)

            j.append(float(vlist[0]))

        m = np.asarray(j)
        tt = np.arctan(m / p2)
        angles = np.degrees(tt)

        #Analise dos angulos. Para verificar os angulos basta descomentar as linhas seguintes
        # inc = (v1/v2)*np.sin(tt)
        # angles_inc = np.arcsin(inc)
        # angles_inc_degree =  np.degrees(angles_inc)
        # print('angulos de transmissao', angles)
        # print('angulos de incidencia', angles_inc_degree)
        # ang_critico2 = np.arcsin(v1/ v2)
        # ang_critico_graus2 = ang_critico2 * 180 / np.pi
        # print('angulo critico=', ang_critico_graus2)


        return angles

    def reflect_travel_time(self, nlayer, thick1, i_l1,v1,  thick2, i_l2, v2):
        geo = []

        if nlayer == 1:
            for i in range(len(i_l1)):
                geo.append(i + 1)
            d = thick1/np.cos(i_l1*np.pi/180)
            t = 2*d/v1

        if nlayer == 2:
            for i in range(len(i_l2)):
                geo.append(i + 1)
            d2= thick2/np.cos(i_l2*np.pi/180)
            t2 = 2 * d2 / v2
            theta1 = np.arcsin((v1/v2)*np.sin(i_l2*np.pi/180))
            d1 = thick1 / np.cos(theta1)
            t1=2*d1/v1
            t = t1+t2

        return(geo, t)




if __name__ == '__main__':
    app = QtWidgets.QApplication(sys.argv)
    dialog = QtWidgets.QMainWindow()
    prog = MyFirstGuiProgram(dialog)

    dialog.show()
    sys.exit(app.exec_())
