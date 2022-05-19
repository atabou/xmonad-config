#!/bin/sh

iwconfig wlo1 2>&1 | grep -q no\ wireless\ extensions\. && {
  echo wired
  exit 0
}

essid=`iwconfig wlo1 | awk -F '"' '/ESSID/ {print $2}'`
stngth=`iwconfig wlo1 | awk -F '=' '/Quality/ {print $2}' | cut -d '/' -f 1`
bars=`expr $stngth / 14`

case $bars in
  0)  bar='<fn=1>\xe2\x96\xad</fn><fn=1>\xe2\x96\xad</fn><fn=1>\xe2\x96\xad</fn><fn=1>\xe2\x96\xad</fn><fn=1>\xe2\x96\xad</fn>' ;;
  1)  bar='<fn=1>\xe2\x96\xac</fn><fn=1>\xe2\x96\xac</fn><fn=1>\xe2\x96\xad</fn><fn=1>\xe2\x96\xad</fn><fn=1>\xe2\x96\xad</fn>' ;;
  2)  bar='<fn=1>\xe2\x96\xac</fn><fn=1>\xe2\x96\xac</fn><fn=1>\xe2\x96\xad</fn><fn=1>\xe2\x96\xad</fn><fn=1>\xe2\x96\xad</fn>' ;;
  3)  bar='<fn=1>\xe2\x96\xac</fn><fn=1>\xe2\x96\xac</fn><fn=1>\xe2\x96\xac</fn><fn=1>\xe2\x96\xad</fn><fn=1>\xe2\x96\xad</fn>' ;;
  4)  bar='<fn=1>\xe2\x96\xac</fn><fn=1>\xe2\x96\xac</fn><fn=1>\xe2\x96\xac</fn><fn=1>\xe2\x96\xac</fn><fn=1>\xe2\x96\xad</fn>' ;;
  5)  bar='<fn=1>\xe2\x96\xac</fn><fn=1>\xe2\x96\xac</fn><fn=1>\xe2\x96\xac</fn><fn=1>\xe2\x96\xac</fn><fn=1>\xe2\x96\xac</fn>' ;;
  *)  bar='[--!--]' ;;
esac

echo -e $bar $essid

exit 0
