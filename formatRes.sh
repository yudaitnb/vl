#!/bin/bash

# 入力ファイル名を指定
input="examples/evaluation.txt"
# 出力ファイル名を指定
output="output.csv"

# ヘッダーを出力ファイルに書き込む
echo "nmod,nver,Parsing,Compiling,Minimize,ConsRes_CPU,ConsRes_Real,ConsRes_sbv_elapsed,Extraction,set-option,define-fun,assert,minimize,check-sat,get-objectives,get-value" > "$output"


# function parse_timestamp () {
#   if [ -p /dev/stdin ]; then
#     timestamp=$(cat /dev/stdin)  # alternative: $(cat -)
#   else
#     timestamp=$1
#   fi

#   timestamp=$0
#   minutes=$(echo $timestamp | cut -d'm' -f1)
#   seconds=$(echo $timestamp | cut -d':' -f2 | cut -d's' -f1)

#   # 分を秒に変換し、それを秒と合計
#   total_seconds=$(echo "$minutes * 60 + $seconds" | bc -l)

#   echo "${total_seconds} s"
# }

# 初期カウントをセット
set_option_count=0
define_fun_count=0
assert_count=0
minimize_count=0
check_sat_count=0
get_objectives_count=0
get_value_count=0

# 入力ファイルを行ごとに読み込む
while IFS= read -r line; do
    # nmodとnverの情報を取得
    if [[ $line =~ "+++ nmod=" ]]; then
        nmod=$(echo "$line" | awk -F"[=, ]+" '{print $3}')
        nver=$(echo "$line" | awk -F"[=, ]+" '{print $5}')
        # カウントをリセット
        set_option_count=0
        define_fun_count=0
        assert_count=0
        minimize_count=0
        check_sat_count=0
        get_objectives_count=0
        get_value_count=0
    fi

    # 各タイムスタンプを取得
    value=$(echo "$line" | awk -F":" '{print $2}' | awk '{print $1}' | sed 's/s//')
    # csvの行を作成
    case $line in
        *"Parsing"* ) parsing=$value ;;
        *"Compiling"* ) compiling=$value ;;
        *"Minimize"* ) minimize=$value ;;
        *"ConsRes (CPU)"* ) consres_cpu=$(echo "$line" | awk '{print $4}' | sed 's/s//') ;;
        *"ConsRes (Real)"* ) consres_real=$value ;;
        # *"SBV Elapsed time"* ) consres_sbv_elapsed=$(echo "$line" | awk -F":" '{print $2":"$3}' | awk -F"[ms:.]" '{min=($2=="" ? $1 : 0); sec=($2=="" ? $3 : $1); subsec=($2=="" ? $4 : $2); printf "%.3f\n", (min*60 + sec + subsec*0.001)}' ) ;;
        *"SBV Elapsed time"* ) consres_sbv_elapsed=$(echo "$line" | awk -F"[hms:.]" '{
              hr=0; min=0; sec=0; subsec=0;

              if (NF == 7) {
                  hr = $1; min = $3; sec = $5; subsec = $6;
              } else if (NF == 5) {
                  min = $1; sec = $3; subsec = $4;S
              } else {
                  sec = $1; subsec = $2;
              }

              printf "%.3f\n", (hr*3600 + min*60 + sec + subsec*0.001);
          }' ) ;;
        *"Extraction"* ) extraction=$value ;;
        *"set-option"* ) set_option_count=$value ;;
        *"define-fun"* ) define_fun_count=$value ;;
        *"assert"* ) assert_count=$value ;;
        *"minimize"* ) minimize_count=$value ;;
        *"check-sat"* ) check_sat_count=$value ;;
        *"get-objectives"* ) get_objectives_count=$value ;;
        *"get-value"* ) get_value_count=$value
            # csvの行を出力ファイルに追加
            echo "$nmod,$nver,$parsing,$compiling,$minimize,$consres_cpu,$consres_real,$consres_sbv_elapsed,$extraction,$set_option_count,$define_fun_count,$assert_count,$minimize_count,$check_sat_count,$get_objectives_count,$get_value_count" >> "$output"
            ;;
    esac
done < "$input"
