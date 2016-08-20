import tkinter
class EraserWaiterView:
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

    def __init__(self, thingtopoll):
        self.__agree = False;
        self.__root = tkinter.Tk()
        self.__root.wm_title('Naraeon SSD Tools - Secure Erase')
        self.__mainloop = self.__root.mainloop
        self.__thingtopoll = thingtopoll
        frame = tkinter.Frame(self.__root)
        frame.pack(fill=tkinter.BOTH)
        self.__initMessageFrame(frame)
        self.__poll()
        self.__root.protocol("WM_DELETE_WINDOW", (lambda : 1-1)) #lambda for NOP
        self.__center()
        self.__mainloop()

    def __initButtonFrame(self, frame):
        self.__buttonFrame = tkinter.Frame(frame)
        self.__buttonFrame.pack(anchor='center', pady=self.__BUTTONPADDING)

    def __initMessageFrame(self, frame):
        self.__messageFrame = tkinter.Frame(frame)
        self.__messageFrame.pack(side=tkinter.TOP, fill=tkinter.BOTH, expand=True)
        tkinter.Label(self.__messageFrame, text='끝날 때까지 기다려주세요.\n'
                                                '창을 끄거나 재시작해도 작업은 중지되지 않습니다.\n'
                                                '만약 강제로 재시작을 시도할 시 장치가 고장납니다.',
                      borderwidth=self.__DESCRIPTIONBORDER)\
            .grid(row=self.__DESCRIPTIONROW, column=self.__NAMECOLUMN)

    def __poll(self):
        if not self.__thingtopoll():
            self.__root.destroy()
        else:
            self.__root.after(500, self.__poll)