class article:
    def __init__(self,title,plot):
        self.title=str(title)
        self.plot=str(plot)
        self.digitsPlot=len(self.plot)
        self.wordsPlot=len(self.plot.split())
        self.digitsTitle=len(self.title)
        self.wordsTitle=len(self.title.split())
        