library(ggplot2)
library(ggrepel) # to może trzeba będzie zainstalować
# alternatywnie bez ggrepel można użyć geom_text(aes(label=gatuenk),hjust=0, vjust=0),
# ale to będzie brzydkie
library(PogromcyDanych)

waga_dl_plot <- ggplot(koty_ptaki, aes(x=koty_ptaki$waga, y=koty_ptaki$dlugosc)) + 
  geom_point(aes(color=koty_ptaki$druzyna == "Kot")) + ggtitle("Waga vs długość") +
  geom_label_repel(aes(label = gatunek), box.padding   = 0.35, point.padding = 0.5,
                   segment.color = 'grey50')

# nie modyfikuję legendy, bo ne było tego w opisie zadania :) :)
plot(waga_dl_plot)
