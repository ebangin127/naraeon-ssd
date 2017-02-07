import tkinter
import processRunner
import eraseType
class CompleteView:
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
        if laststate != eraseType.EraseType.closed:
            self.__agree = False;
            self.__root = tkinter.Tk()
            self.__root.wm_title('Naraeon SSD Tools - Secure Erase')
            self.__mainloop = self.__root.mainloop
            frame = tkinter.Frame(self.__root)
            frame.pack(fill=tkinter.BOTH)
            self.__initMessageFrame(frame, laststate, model, serial)
            self.__initButtonFrame(frame)
            self.__initButton()
            self.__center()
            self.__mainloop()

    def __initButtonFrame(self, frame):
        self.__buttonFrame = tkinter.Frame(frame)
        self.__buttonFrame.pack(anchor='center', pady=self.__BUTTONPADDING)

    def __initMessageFrame(self, frame, laststate, model, serial):
        self.__messageFrame = tkinter.Frame(frame)
        self.__messageFrame.pack(side=tkinter.TOP, fill=tkinter.BOTH, expand=True)
        if serial is not None:
            serial = 'Serial: ' + serial
        else:
            serial = ''
        if laststate:
            tkinter.Label(self.__messageFrame, text='Secure erase has been completed. \n'
                                                    'Press Restart. \n\n'
                                                    'Model: ' + model + '\n' + serial,
                          borderwidth=self.__DESCRIPTIONBORDER)\
                .grid(row=self.__DESCRIPTIONROW, column=self.__NAMECOLUMN)
        else:
            tkinter.Label(self.__messageFrame, text='Secure erase has been FAILED. \n'
                                                    'Press Restart and retry. \n\n'
                                                    'Model: ' + model + '\n' + serial,
                          borderwidth=self.__DESCRIPTIONBORDER)\
                .grid(row=self.__DESCRIPTIONROW, column=self.__NAMECOLUMN)

    def __initButton(self):
        ok = tkinter.Button(self.__buttonFrame, text='Restart')
        ok.pack(side=tkinter.LEFT, padx=self.__BUTTONPADDING)
        ok.bind('<Button-1>', self.__okClick)

    def __okClick(self, event):
        runner = processRunner.ProcessRunner()
        runner.run('shutdown -r now')
        self.__root.destroy()
