## Weiderechner allgemein

Der Weiderechner stellt eine einfache Grundlage zur Weideplanung dar.
Die Fütterung im Stall kann in Kombination mit der Weide betrachtet
werden. Die Eingabe unterschiedlicher Szenarien ermöglicht den Vergleich
zwischen Fütterungsstrategien. Die Ausgabe zeigt eine einfache Bewertung
der Ration zur Überwachung der Versorgung der Kuh. Dabei wird die
Energieversorgung und die Faserversorgung (NDF, ADF, NFC)
berücksichtigt. Die Berechnung ersetzt keine Rationsplanung, da weitere
wichtige Faktoren wie pysikalisch effektive NDF nicht berücksichtigt
werden. Der Bedarf der Herde wird außerdem im Kontext der Stallfütterung
auf die Weide übertragen. Die benötigte Gesamtweidefläche bei
unterschiedlichen Wachstumsraten wird dafür ausgegeben. Auch hier lässt
die Eingabe der unterschiedlichen Sznearien eine Planung für saisonale
Veränderungen zu. Entsprechend kann bei unterschiedlicher Stallfütterung
evaluiert werden ob bei unterschiedlichen Wachstumsraten ausreichend
Weidefläche durch die betriebelichen Gegebenheiten vorliegt. Im
Leitfaden des Projekts finden sich weiterführende Informationen zur
Fütterung, zum Bedarf der weidenden Kuh und zum Begriff Wachstumsrate.
“LINKS EINFÜGEN”.

#### Hintergrund der Berechnungen

Die Futteraufnahme der Kühe wird mit dem Modell von Gruber et al. 2004
geschätzt. Hierbei werden Leistungs-, Tier-, Laktations- und
Futterparameter berücksichtigt. bei hohem Futterangebot im Stall kann
die angebotene Futtermenge nach Abzug des Futterrests die Futteraufnahme
überschreiten. Dies begründet sich durch das Modell. Für die Berechnung
der Weidefläche wird dadurch ein Wert von 0 ha ausgegeben, es kann daher
eine Auslauf- oder Joggingweide angenommen werden. Die Beurteilung der
Futterinhaltsstoffe werden die üblichen Werte zum Energiebedarf
angenommen. Für die Faserbewertung werden die Richtwerte für NDF &gt;300
g/ kg TS, für ADF &gt;200 g/ kg TS und NFC &lt;400 g/ kg TS angenommen.
Eine Anpassung für geringe NDF Werte im Grundfutter oder erhöhte NFC
Werte wird nicht vorgenommen.

## Parzelleneinteilung und Messung Futtermenge

Für Betriebe, die intensive Umtriebsweidesysteme nutzen, bietet der
Weiderechner eine Anwendung zur Planung der Parzellengröße. Hierfür ist
eine Messung des Weideaufwuchses mit einem Rising Plate Meter oder
ähnlichen Hilfsmitteln nötig (“LINK”). Werden andere Hilfsmittel
genutzt, müssen die Messungen in die entsprechende Höhe aus dem Rising
Plate Meter umgewandelt werden “LINK”. Im Weiderechner können die
angestrebte Aufwuchshöhe und der Zielweiderest “LINK” angegeben werden.
Für fortgeschrittene Nutzer:innen existiert die Möglichkeit eine eigene
Formel zur Schätzung der Futtermenge in kg TS/ha aus der
Aufwuchshöhenmessung zu einzugeben. Die Standardformel wurde im MuD
Projekt Verbesserung des Tierwohls bei der Weidehaltung von Milchkühen
erstellt. Typische Formeln sind:

TS (kg/ha)= 239x-100   (Grünlandzentrum, Deutschland)  
TS (kg/ha)= 250x     (Teagasc,Irland)  
TS (kg/ha)= 280x+500   (DairyNZ, Neuseeland)  
TS (kg/ha)= 240x-767   (Agroscope, Schweiz)  

wobei x der komprimierten Aufwuchshöhe aus der Rising Plate Meter
Messung in cm entspricht.

## Quellen und Links

Jeroch, H., Drochner, W., Rodehutscord, M., Simon, A., Simon, O.,
Zentek, J. (Eds.), 2020. Ernährung landwirtschaftlicher Nutztiere:
Ernährungsphysiologie, Futtermittelkunde, Fütterung, 3., vollständig
überarbeitete und erweiterte Auflage. ed, UTB Agrarwissenschaften,
Veterinärmedizin. Verlag Eugen Ulmer, Stuttgart.  
National Research Council (U.S.) (Ed.), 2001. Nutrient requirements of
dairy cattle, 7th rev. ed. ed. National Academy Press, Washington,
D.C.  
Gruber, L., Schwarz, F., Erdin, D., Fischer, B., Spiekers, H.,
Steingass, H., Meyer, U., Chassot, A., Jilg, T., Omermaier, A.,
Gruggenberg, T., 2004. Vorhersage der Futteraufnahme von Milchkühen -
Datenbasis von 10 Forschungs-und Universitätsinstituten Deutschlands.
VDLUFA Schriftenr 60, 484–504.  
Bayrische Landesanstalt für Landwirtschaft, 2021. Gruber Tabelle zur
Fütterung der Milchkühe, Zuchtrinder, Schafe, Ziegen. 47. Auflage, Stand
2021. Freising-Weihenstephan  
LAZ-BW, 2022.Uni-Rat-Rationsberechnungsprogramm für Rinder, Schafe und
Ziegen. LAzBW, Aulendorf  
Schori, F., 2020. Mit Herbometer und Pasturemeter die Wuchshöhe von
Weiden messen und die Grasmasse schätzen.
<a href="https://doi.org/10.34776/AFS11-46\"
class="uri">https://doi.org/10.34776/AFS11-46\</a>
<https://www.agrarforschungschweiz.ch/wp-content/uploads/2020/02/046-052_Schori_D_def.pdf>
Teagasc, 2017. How to measure grass.  
<a
href="https:\//www.teagasc.ie/media/website/crops/grassland/How-to-measure-grass-right.pdf"
class="uri">https:\//www.teagasc.ie/media/website/crops/grassland/How-to-measure-grass-right.pdf</a>  
McCarthy, S., Wims, C., Lee, J.M., Donaghy, D., 2010. Perennial rygrass
management in spring, DairyNZ.  
<a
href="https://www.dairynz.co.nz/feed/pasture/assessing-and-allocating-pasture/pasture-assessment/\"
class="uri">https://www.dairynz.co.nz/feed/pasture/assessing-and-allocating-pasture/pasture-assessment/\</a>
