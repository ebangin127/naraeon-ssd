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
        messageText = 'When you press OK, you cannot go back!! \n ' \
                      'Double check if it\'s correct. \n\n' \
                      'Model: ' + model + '\n'
        if serial is not None:
            messageText += 'Serial: ' + serial

        tkinter.Label(self.__messageFrame, text=messageText, borderwidth=self.__DESCRIPTIONBORDER)\
            .grid(row=self.__DESCRIPTIONROW, column=self.__NAMECOLUMN)

    def __initButton(self):
        ok = tkinter.Button(self.__buttonFrame, text='Correct')
        ok.pack(side=tkinter.LEFT, padx=self.__BUTTONPADDING)
        ok.bind('<Button-1>', self.__okClick)
        cancel = tkinter.Button(self.__buttonFrame, text='Not correct')
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