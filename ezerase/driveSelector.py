import tkinter
class DriveSelector:
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
    def __initButton(self):
        ok = tkinter.Button(self.__buttonFrame, text='확인')
        ok.pack(side=tkinter.LEFT, padx=self.__BUTTONPADDING)
        cancel = tkinter.Button(self.__buttonFrame, text='취소')
        cancel.pack(side=tkinter.LEFT, padx=self.__BUTTONPADDING)
    def __init__(self):
        root = tkinter.Tk()
        root.wm_title('초기화')
        self.__mainloop = (lambda: root.mainloop())
        frame = tkinter.Frame(root)
        frame.pack(fill=tkinter.BOTH)
        self.__initRadioFrame(frame)
        self.__initButtonFrame(frame)
        self.__initGrid()
        self.__initButton()
    def select(self, findresult):
        currentRow = 1;
        for device in findresult:
            radio = tkinter.Radiobutton(self.__radioFrame)
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