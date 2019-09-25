from article import article
import csv

def main():
    f1 = open('plots.txt', 'r')
    f2 = open('titles.txt','r')

    f_plots = f1.read()
    f_titles= f2.read()

    plots = f_plots.split("<EOS>")
    titles = f_titles.split("\n")

    articles = []
    for i in range(len(titles)):
        articles.append(article(titles[i],plots[i]))

    csvData = []
    csvData.append(["Title","NW_Title","ND_Title","NW_Plot","ND_Plot"])
    for i in range(len(titles)):
        csvData.append([articles[i].title,articles[i].wordsTitle,articles[i].digitsTitle,articles[i].wordsPlot,articles[i].digitsPlot])
    csv.register_dialect('mD',quoting=csv.QUOTE_ALL,skipinitialspace=True)

    with open('dataset2.csv', 'w') as csvFile:
        writer = csv.writer(csvFile,dialect='mD')
        writer.writerows(csvData)
    csvFile.close()

main()