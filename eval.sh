#!/bin/bash
expath="/home/yudaitnb/vl/examples"
name="Test"
ext=".hs"
nmod=2
nver=1

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
      # 依存グラフ葉モジュールのケースはmodule名を追加
      cat "/home/yudaitnb/vl/examples/List/1.0.0/List.hs" | sed "1,3d" | sed "1imodule $namei where" > $filepath
    else
      # 依存グラフ内部モジュールの場合はmodule名とimport宣言を追加
      cat "/home/yudaitnb/vl/examples/List/1.0.0/List.hs" | sed "1,3d" | sed "1imodule $namei where" | sed "2iimport $nameis" > $filepath
    fi
  done
  # ゴミ掃除
  # rm -rf $modpath
done