import tkinter
class SelectorView:
    __DESCRIPTIONBORDER = 10
    __DESCRIPTIONROW = 0
    __BUTTONPADDING = 5
    __RADIOCOLUMN = 0
    __NAMECOLUMN = 1
    __MODELCOLUMN = 2
    __SIZECOLUMN = 3
    __SERIALCOLUMN = 4

    def __initRadioFrame(self, frame):
        self.__radioFrame = tkinter.Frame(frame)
        self.__radioFrame.pack(side=tkinter.TOP, fill=tkinter.BOTH, expand=True)

    def __initButtonFrame(self, frame):
        self.__buttonFrame = tkinter.Frame(frame)
        self.__buttonFrame.pack(anchor='center', pady=self.__BUTTONPADDING)

    def __initGrid(self):
        tkinter.Label(self.__radioFrame, text='경로', borderwidth=self.__DESCRIPTIONBORDER)\
            .grid(row=self.__DESCRIPTIONROW, column=self.__NAMECOLUMN)
        tkinter.Label(self.__radioFrame, text='모델명', borderwidth=self.__DESCRIPTIONBORDER)\
            .grid(row=self.__DESCRIPTIONROW, column=self.__MODELCOLUMN)
        tkinter.Label(self.__radioFrame, text='크기', borderwidth=self.__DESCRIPTIONBORDER)\
            .grid(row=self.__DESCRIPTIONROW, column=self.__SIZECOLUMN)
        tkinter.Label(self.__radioFrame, text='시리얼 번호', borderwidth=self.__DESCRIPTIONBORDER)\
            .grid(row=self.__DESCRIPTIONROW, column=self.__SERIALCOLUMN)

    def __okClick(self, event):
        if self.__selected.get() != '':
            self.__root.destroy()

    def __cancelClick(self, event):
        self.__selected = tkinter.StringVar()
        self.__root.destroy()

    def __initButton(self):
        ok = tkinter.Button(self.__buttonFrame, text='확인')
        ok.pack(side=tkinter.LEFT, padx=self.__BUTTONPADDING)
        ok.bind('<Button-1>', self.__okClick)
        cancel = tkinter.Button(self.__buttonFrame, text='취소')
        cancel.pack(side=tkinter.LEFT, padx=self.__BUTTONPADDING)
        cancel.bind('<Button-1>', self.__cancelClick)

    def __center(self):
        self.__root.withdraw()
        self.__root.update_idletasks()
        x = (self.__root.winfo_screenwidth() - self.__root.winfo_reqwidth()) / 2
        y = (self.__root.winfo_screenheight() - self.__root.winfo_reqheight()) / 2
        self.__root.geometry("+%d+%d" % (x, y))
        self.__root.deiconify()

    def __init__(self):
        self.__root = tkinter.Tk()
        self.__root.wm_title('Secure erase')
        self.__mainloop = self.__root.mainloop
        frame = tkinter.Frame(self.__root)
        frame.pack(fill=tkinter.BOTH)
        self.__initRadioFrame(frame)
        self.__initButtonFrame(frame)
        self.__initGrid()
        self.__initButton()
        self.__center()

    def select(self, findresult):
        currentRow = 1
        self.__selected = tkinter.StringVar()
        for device in findresult:
            radio = tkinter.Radiobutton(self.__radioFrame, variable=self.__selected, value=device['kname'])
            radio.grid(row=currentRow, column=self.__RADIOCOLUMN)
            kname = tkinter.Label(self.__radioFrame, text=device['kname'])
            kname.grid(row=currentRow, column=self.__NAMECOLUMN)
            model = tkinter.Label(self.__radioFrame, text=device['model'])
            model.grid(row=currentRow, column=self.__MODELCOLUMN)
            size = tkinter.Label(self.__radioFrame, text=device['size'])
            size.grid(row=currentRow, column=self.__SIZECOLUMN)
            serial = tkinter.Label(self.__radioFrame, text=device['serial'])
            serial.grid(row=currentRow, column=self.__SERIALCOLUMN)
            currentRow += 1
        self.__mainloop()

    def selected(self):
        return self.__selected.get()