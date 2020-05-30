# Procesado de secuencias MIDI con R (I). Decodificación
# www.overfitting.net
# https://www.overfitting.net/2018/01/procesado-de-secuencias-midi-con-r-i.html

library(tuneR)
library(data.table)
library(tiff)


# DECODIFICACIÓN ARCHIVO MIDI

mzmidi=readMidi("mazinger09.mid")  # Eventos MIDI
mznotes=data.table(getMidiNotes(mzmidi))  # Solo notas:
# (time, length, track, channel, note, notename, velocity)
mznotes$channel=mznotes$channel+1  # Canales MIDI 0..15 -> 1..16
summary(mznotes)  # Estructura fichero MIDI

# Ploteamos correspondencia pistas/canales MIDI
INFTr=min(mznotes$track)
SUPTr=max(mznotes$track)

plot(mznotes$track, mznotes$channel, main='MIDI Channel matching',
  xlab=paste0('Track (',INFTr,'-',SUPTr,')'), ylab='Channel (1-16)',
  xlim=c(INFTr,SUPTr), ylim=c(1,16),
  col='red', pch = 16, cex=1.5, axes=F)
abline(v=c(INFTr:SUPTr), h=c(1:16), col='gray', lty='dotted')
axis(1, at=c(INFTr:SUPTr), cex.axis=0.7)
axis(2, at=c(1:16), cex.axis=0.7)

# Nos quedamos con una repetición de la melodía principal
BPM=185  # beats/min
BEAT=480  # time de 1 beat
COMPAS=BEAT*4  # time de 1 compás = 4 beats

melodia=mznotes[which(mznotes$track==10), ]  # Pista melodía
# Ajustamos inicio de la melodía con 1 compás (4 beats) vacío
melodia$time=melodia$time-min(melodia$time)+COMPAS*1
# El estribillo abarca 30 compases, más compás en blanco inicial
melodia=melodia[which(melodia$time<=COMPAS*31)]

# Ploteamos melodía
INFTime=min(melodia$time)
SUPTime=max(melodia$time)

plot(melodia$time, melodia$note, xlim=c(INFTime,SUPTime),
  main='Melodía', xlab='MIDI Time', ylab='MIDI Note',
  col='red', pch = 16,  cex=0.8)
lines(melodia$time, melodia$note, lty='dotted', type='s', col='gray') 


# VISUALIZACIÓN PISTA MIDI

# note=nota -> radio
# time=tiempo -> ángulo inicial
# length=duración -> delta angular

# Normalizamos a valores [0..1]
melodia$time=melodia$time-min(melodia$time)
# max(melodia$time) = 55680 -> 60000=360º
melodia$time=melodia$time/60000
melodia$length=melodia$length/60000  # Mismo escalado que tiempos
# Nota más baja con radio>0
melodia$note=melodia$note-min(melodia$note)+6 
melodia$note=melodia$note/max(melodia$note)

LADO=512
SOLAPET=8/100  # Solape temporal (% de 360º)
SOLAPER=20/100  # Solape radial (100% el radio llega al centro)
visualizacionmidi=array(0, c(LADO,LADO))

for (i in 1:nrow(melodia)) {
  nota=melodia$note[i]
  tiempo=melodia$time[i]
  duracion=melodia$length[i]

  for (x in 1:LADO) {
    for (y in 1:LADO) {
      X=x-LADO/2
      Y=y-LADO/2
      if (X!=0 | Y!=0) {  # Evitamos (0,0)
        R=(X^2+Y^2)^0.5
        if (X<0 & Y<0) {
            Alpha=asin(-Y/R)+pi
          } else if (X<0) {
            Alpha=acos(X/R)
          } else {
            Alpha=asin(Y/R)
          }
        if (Alpha<0) {Alpha=Alpha+2*pi}
        if (R>=(nota-SOLAPER)*LADO/2 & R<=nota*LADO/2 &
          Alpha>=tiempo*2*pi &
          Alpha<=(tiempo+duracion+SOLAPET)*2*pi)
          {visualizacionmidi[x,y]=
              visualizacionmidi[x,y]+1}
      }
    }
  }
}
visualizacionmidi[LADO/2,LADO/2]=1  # Referencia para ejes

writeTIFF((visualizacionmidi/max(visualizacionmidi))^(1/2.2),
  "visualizacionmidi.tif", bits.per.sample=16, compression="LZW")
