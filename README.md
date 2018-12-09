# Schreibe Dein Programm!

Dies sind die Quellen zum Buch "Schreibe Dein Programm!" von Herbert
Klaeren und Michael Sperber.

Die Webseite zum Buch ist bei:

[http://deinprogramm.de/](http://deinprogramm.de/)

# PDF erzeugen

Das Buch ist in LaTeX geschrieben.

Zum Bauen ist eine TexLive-Distribution nötig. Unter Debian/Ubuntu installiert man am einfachsten das "full"-package
```
sudo apt-get -qq update && sudo apt-get install -y --no-install-recommends texlive-full
```

Zum Bauen sind folgende Befehle notwendig:

```
touch includeonly.tex
pdflatex i1
bibtex i1
pdflatex i1
sed -i 's/\\"/"/g' i1.idx
sed -i 's/\\eingebaut/eingebaut/g' i1.idx
makeindex -c -s i1.mst -g i1
pdflatex i1
```

Fertig ist das Buch in `i1.pdf`.

Wer aktiv am Buch schreibt, mag u.U. ein `includeonly`-Statement in
`includeonly.tex` schreiben, damit nicht immer alles geTeXt wird.

# Lizenz

Dieses Buch ist lizensiert unter der Creative-Commons-Lizenz
[Namensnennung 4.0 International (CC BY
4.0)](http://creativecommons.org/licenses/by/4.0/deed.de).

