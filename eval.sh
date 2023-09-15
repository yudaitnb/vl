#!/bin/bash
expath="examples"
resultpath="${expath}/evaluation.txt" 
logpath="${expath}/Main.log"
name="Test"
ext=".hs"

##### SETTING #####
nmodstart=2
nmodend=2
nverstart=3
nverend=3
iteration=1
###################

function count_SMTLib2script_command () {
  arg=$1
  grep -o -i $1 "${expath}/SMTlib2script.out" | wc -l | awk '{printf "%-15s : %s\n", "'"$arg"'", $0}' >> $resultpath
}

# ゴミ掃除
rm -rf $resultpath
for iter in `seq 1 ${iteration}` ; do
  for nver in `seq ${nverstart} ${nverend}` ; do
    for nmod in `seq ${nmodstart} ${nmodend}` ; do
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
        cat "${expath}/MainEval.hs" | sed "1,2d" | sed "1imodule Main where" | sed "2iimport $namei" > "$expath/Main.hs"
        fi

        for v in `seq 1 ${nver}` ; do
          ver="${v}.0.0"
          verpath="$modpath/$ver"
          filepath="$verpath/$namei$ext"
          mkdir $verpath
          if [ $i = $nmod ]
          then 
            # 依存グラフ葉モジュールの場合はmodule名のみを追加(import宣言無し)
            cat "${expath}/${name}.hs" | sed "1,3d" | sed "1imodule $namei where" > $filepath
          else
            # 依存グラフ内部モジュールの場合はmodule名とimport宣言を追加
            cat "${expath}/${name}.hs" | sed "1,3d" | sed "1imodule $namei where" | sed "2iimport $nameis" > $filepath
          fi
          # str="_n${i}_v${v}00"
          str="_n${i}"
          sed -i "s/@/${str}/g" $filepath
        done
      done

      stack run "Main.hs"

      echo "+++ nmod=${nmod}, nver=${nver} +++" >> $resultpath
      tail -n 7 $logpath >> $resultpath
      cat $logpath | grep "SBV Elapsed time" >> $resultpath
      count_SMTLib2script_command "(set-option"
      count_SMTLib2script_command "(define-fun"
      count_SMTLib2script_command "(assert"
      count_SMTLib2script_command "(minimize"
      count_SMTLib2script_command "(check-sat"
      count_SMTLib2script_command "(get-objectives"
      count_SMTLib2script_command "(get-value"

      # ゴミ掃除
      for i in `seq 1 ${nmod}` ; do
        namei="${name}${i}"
        modpath="$expath/$namei"
        rm -rf $modpath
      done
    done
  done
done
