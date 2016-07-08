import tkinter
class EraserWaiterView:
    __DESCRIPTIONBORDER = 10
    __DESCRIPTIONROW = 0
    __BUTTONPADDING = 5
    __NAMECOLUMN = 1

    def __init__(self, thingtopoll):
        self.__agree = False;
        self.__root = tkinter.Tk()
        self.__root.wm_title('Notice')
        self.__mainloop = self.__root.mainloop
        self.__thingtopoll = thingtopoll
        frame = tkinter.Frame(self.__root)
        frame.pack(fill=tkinter.BOTH)
        self.__initMessageFrame(frame)
        self.__poll()
        self.__root.protocol("WM_DELETE_WINDOW", (lambda : 1-1)) #lambda for NOP
        self.__mainloop()

    def __initButtonFrame(self, frame):
        self.__buttonFrame = tkinter.Frame(frame)
        self.__buttonFrame.pack(anchor='center', pady=self.__BUTTONPADDING)

    def __initMessageFrame(self, frame):
        self.__messageFrame = tkinter.Frame(frame)
        self.__messageFrame.pack(side=tkinter.TOP, fill=tkinter.BOTH, expand=True)
        tkinter.Label(self.__messageFrame, text='Please wait for completion.\n'
                                                'You can\'t undo what you did.\n'
                                                'So DO NOT RESTART\n'
                                                'It does nothing but BRICK YOUR SSDs.',
                      borderwidth=self.__DESCRIPTIONBORDER)\
            .grid(row=self.__DESCRIPTIONROW, column=self.__NAMECOLUMN)

    def __poll(self):
        if not self.__thingtopoll():
            self.__root.destroy()
        else:
            self.__root.after(500, self.__poll)