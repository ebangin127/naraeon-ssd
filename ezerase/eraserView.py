import tkinter
class EraserView:
    __DESCRIPTIONBORDER = 10
    __DESCRIPTIONROW = 0
    __BUTTONPADDING = 5
    __NAMECOLUMN = 1

    def __init__(self):
        self.__agree = False;
        self.__root = tkinter.Tk()
        self.__root.wm_title('Notice')
        self.__mainloop = self.__root.mainloop
        frame = tkinter.Frame(self.__root)
        frame.pack(fill=tkinter.BOTH)
        self.__initMessageFrame(frame)
        self.__initButtonFrame(frame)
        self.__initButton()
        self.__mainloop()

    def __initButtonFrame(self, frame):
        self.__buttonFrame = tkinter.Frame(frame)
        self.__buttonFrame.pack(anchor='center', pady=self.__BUTTONPADDING)

    def __initMessageFrame(self, frame):
        self.__messageFrame = tkinter.Frame(frame)
        self.__messageFrame.pack(side=tkinter.TOP, fill=tkinter.BOTH, expand=True)
        tkinter.Label(self.__messageFrame, text='For reset the SSD\'s frozen status, \n'
                                                'this app will place the system into sleep state',
                      borderwidth=self.__DESCRIPTIONBORDER)\
            .grid(row=self.__DESCRIPTIONROW, column=self.__NAMECOLUMN)

    def __initButton(self):
        ok = tkinter.Button(self.__buttonFrame, text='확인')
        ok.pack(side=tkinter.LEFT, padx=self.__BUTTONPADDING)
        ok.bind('<Button-1>', self.__okClick)
        cancel = tkinter.Button(self.__buttonFrame, text='취소')
        cancel.pack(side=tkinter.LEFT, padx=self.__BUTTONPADDING)
        cancel.bind('<Button-1>', self.__cancelClick)

    def __okClick(self, event):
        self.__agree = True;
        self.__root.destroy()

    def __cancelClick(self, event):
        self.__agree = False;
        self.__root.destroy()

    def agree(self):
        return self.__agree;