# !/bin/sh
# =============================================================================
# Wrote by Valerii Grazhdankin, (c) 2022
# For Use in MacOS:
# brew install wla-dx
# =============================================================================
#
ORIG_16K=RK86-16.rom
ORIG_32K=RK86-32.rom
NAME=rk86-monitor
BUILD_DIR="./build"
#
echo
echo "BUILD PHASE"
mkdir -p $BUILD_DIR
mkdir -p ./bin
cd $BUILD_DIR
rm -f $NAME.o $NAME.bin $NAME.hex
# BUILD 16K
wla-8080 -i -v2 -D MEM_SIZE=4000h -o $NAME.o ../src/$NAME.asm
wlalink -r -v ../link.cfg $NAME-16k.bin
# BUILD 32K
wla-8080 -i -v2 -D MEM_SIZE=8000h -o $NAME.o ../src/$NAME.asm
wlalink -r -v ../link.cfg $NAME-32k.bin
#
xxd -c 1 $NAME-16k.bin > $NAME-16k.hex
xxd -c 1 $NAME-32k.bin > $NAME-32k.hex
#
xxd -c 1 ../original/$ORIG_16K > $ORIG_16K.hex
xxd -c 1 ../original/$ORIG_32K > $ORIG_32K.hex

# Compare and print differencies
# echo "ORIGINAL MONITORS 16K and 32K diffs:"
# diff -y -W30 $ORIG_16K.hex $ORIG_32K.hex | grep " |" | sed "s/ |.*: //g"
#
echo
echo "QUALITY CHECK PHASE"
diff -y -W30 $ORIG_16K.hex $NAME-16k.hex | grep " |" | sed "s/ |.*: //g" > ORIG-16K_BUILD-16K.diff
LINES=$(wc -l < ORIG-16K_BUILD-16K.diff | sed "s/ //g" )
if [ "$LINES" == "0" ]; then LINES="NO"; fi
echo "ORIGINAL MONITOR 16K and BUILD 16K diffs: $LINES"
cat ORIG-16K_BUILD-16K.diff
#
diff -y -W30 $ORIG_32K.hex $NAME-32k.hex | grep " |" | sed "s/ |.*: //g" > ORIG-32K_BUILD-32K.diff
LINES=$(wc -l < ORIG-16K_BUILD-16K.diff | sed "s/ //g")
if [ "$LINES" == "0" ]; then LINES="NO"; fi
echo "ORIGINAL MONITOR 32K and BUILD 32K diffs: $LINES"
cat ORIG-32K_BUILD-32K.diff
#
rm -f ../bin/$NAME-*.bin
cp $NAME-*.bin ../bin
cd ..
