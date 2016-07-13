import tkinter
class ConfirmView:
    __DESCRIPTIONBORDER = 10
    __DESCRIPTIONROW = 0
    __BUTTONPADDING = 5
    __NAMECOLUMN = 1

    def __center(self):
        self.__root.withdraw()
        self.__root.update_idletasks()
        x = (self.__root.winfo_screenwidth() - self.__root.winfo_reqwidth()) / 2
        y = (self.__root.winfo_screenheight() - self.__root.winfo_reqheight()) / 2
        self.__root.geometry("+%d+%d" % (x, y))
        self.__root.deiconify()

    def __init__(self, laststate, model, serial):
        self.__agree = False;
        if laststate:
            self.__root = tkinter.Tk()
            self.__root.wm_title('Naraeon SSD Tools - Secure Erase')
            self.__mainloop = self.__root.mainloop
            frame = tkinter.Frame(self.__root)
            frame.pack(fill=tkinter.BOTH)
            self.__initMessageFrame(frame, model, serial)
            self.__initButtonFrame(frame)
            self.__initButton()
            self.__center()
            self.__mainloop()

    def __initButtonFrame(self, frame):
        self.__buttonFrame = tkinter.Frame(frame)
        self.__buttonFrame.pack(anchor='center', pady=self.__BUTTONPADDING)

    def __initMessageFrame(self, frame, model, serial):
        self.__messageFrame = tkinter.Frame(frame)
        self.__messageFrame.pack(side=tkinter.TOP, fill=tkinter.BOTH, expand=True)
        tkinter.Label(self.__messageFrame, text='여기서 확인을 누르면 절대 되돌릴 수 없습니다!! \n'
                                                '다음 사항을 꼭 확인해주세요. \n\n'
                                                '모델명: ' + model + '\n'
                                                '시리얼 번호: ' + serial,
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