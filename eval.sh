#!/bin/bash
expath="/home/yudaitnb/vl/examples"
resultpath="${expath}/evaluation.txt" 
logpath="${expath}/Main.log"
name="Test"
ext=".hs"

##### SETTING #####
nmodlim=5
nverlim=5
###################

# ゴミ掃除
rm -rf $resultpath

for nver in `seq 1 ${nverlim}` ; do
  for nmod in `seq 1 ${nmodlim}` ; do
    # ゴミ掃除
    for i in `seq 1 ${nmod}` ; do
      namei="${name}${i}"
      modpath="$expath/$namei"
      rm -rf $modpath
    done

    # 実験用モジュール生成
    for i in `seq 1 ${nmod}` ; do
      namei="${name}${i}"
      nameis="${name}$((${i}+1))"
      modpath="$expath/$namei"
      mkdir $modpath

      # Main.hsは一つ目のモジュールのみに依存
      if [ $i = 1 ]
      then
      cat "/home/yudaitnb/vl/examples/MainEval.hs" | sed "1,2d" | sed "1imodule Main where" | sed "2iimport $namei" > "$expath/Main.hs"
      fi

      for v in `seq 1 ${nver}` ; do
        ver="${v}.0.0"
        verpath="$modpath/$ver"
        filepath="$verpath/$namei$ext"
        mkdir $verpath
        if [ $i = $nmod ]
        then 
          # 依存グラフ葉モジュールの場合はmodule名のみを追加(import宣言無し)
          cat "/home/yudaitnb/vl/examples/Test.hs" | sed "1,3d" | sed "1imodule $namei where" > $filepath
        else
          # 依存グラフ内部モジュールの場合はmodule名とimport宣言を追加
          cat "/home/yudaitnb/vl/examples/Test.hs" | sed "1,3d" | sed "1imodule $namei where" | sed "2iimport $nameis" > $filepath
        fi
        str="_n${i}_v${v}00"
        sed -i "s/@/${str}/g" $filepath
      done
    done

    stack run "Main.hs"

    echo "+++ nmod=${nmod}, nver=${nver} +++" >> $resultpath
    tail -n 6 $logpath >> $resultpath

    # ゴミ掃除
    for i in `seq 1 ${nmod}` ; do
      namei="${name}${i}"
      modpath="$expath/$namei"
      rm -rf $modpath
    done
  done
done