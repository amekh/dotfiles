;;;
;;; /u/tetsu/src/elisp/header2.el
;;; created: December 12,1994 Monday 12:55:17
;;; modified: December 19,1994 Monday 13:47:29
;;; modified: December 19,1994 Monday 15:57:06
;;; modified: January 24,2001 Wednesday 21:50:11
;;; modified: November 15,2002 Friday 00:31:48
;;; author: tetsu(WATANABE Tetsuya)
;;;
;;; v1.6 機能追加 日付フォーマット
;;; v1.5 機能追加 フルパスとファイル名だけ 日付フォーマット
;;; v1.4 コメントの調整
;;; v1.3 モードからも候補を
;;; v1.2 少し効率アップ
;;; v1.1 bug fix [SUFFIX]
;;; v1.0

;; ■ 目的
;;
;; ファイルのヘッダ部分をできるだけ簡単に生成するためのものです。テン
;; プレートファイルを用意することで、簡単に追加修正が可能になっていま
;; す。新しいモードに対応したい場合も、そのファイルの拡張子に合わせた
;; テンプレートを用意するだけです。拡張子がない場合や、テンプレートが
;; ない場合には、デフォルトのテンプレートファイル(misc)が使用されます。
;;
;; 定型のヘッダは楽して作成しましょう。プログラムを作成しているときに
;; は、いちいち手で細かな情報を書くのは面倒です。こういうことを自動的
;; にやらせましょう。
;;
;; ヘッダも何もないソースが散らかっていませんか? この header2 を使用し、
;; 定型部分を自動生成し、ソースコード自身についてすぐに書き出せるよう
;; にしましょう。
;;
;; 思い付いたアイディアを忘れないうちにコメントとして書き出しておくこ
;; とや、機能の概要や、依存するもの関連するものをまとめておくことは、
;; 後々役立つものです。

;; ■ インストール
;;
;; Emacs-Lisp のパスに header2.el を用意しておきます。byte-compile は、
;; 必ずしも必要ではありませんが、実施した方がいいでしょう。
;;
;;   M-x byte-compile-file
;;
;; インストールについて詳しくは、システム上で GNU Emacs(Mule) を管理さ
;; れている方に聞いてください。

;; ■ 使い方
;;
;; どこでもすぐに使えるように、~/.emacs でロードします。
;;
;;   ~/.emacs のサンプル
;;   (load "header2")

;; ■ キーバインディング
;;
;; C-c H でヘッダの作成
;; C-u C-c H で修正用ヘッダの挿入
;; C-c T で時間の文字列の挿入

;; ■ 使う前に用意するもの
;; 
;; ~/lib/header というディレクトリにファイルの拡張子に合わせてテンプ
;; レートファイルを作成します。
;;
;; テンプレートファイルで使用するキーワードは次のようなものです。
;; 
;; [FULLPATH]  ファイル名 -> full path
;; [FILENAME]  ファイル名 -> ファイル名のみ
;; [AUTHOR]    作者       -> login name(full name)
;; [DATE]      日付       -> December 19,1994 Monday 14:23:08
;; [YYYYMMDD]  日付       -> 2002/11/12 11:26:10
;; [SUFFIX]    拡張子     -> sample.c なら c
;;
;; テンプレートファイルについては、もう少し後ろの方で。

;; ■ グローバル変数
;; 
;; header-template-dir
;; header-item-alist
;;
;; header で動作をコントロールするための変数はこの二つになっています。
;; 詳しい説明は describe-variable で確認できます。

;; ■ テンプレートファイルについて
;;
;; 定型ヘッダファイルを作成するための、もととなるものがテンプレートファ
;; イルです。
;;
;; テンプレートファイルは、header-template-dir で指定されたディレクト
;; から次の条件で選びだされます。
;;
;;   1. 編集しているファイルの拡張子と同じ名前のテンプレートファイル
;;   2. 編集しているファイルが持つモードから、auto-mode-alist で定義
;;      されている拡張子の情報を利用し、その定義されている拡張子と同
;;      じ名前のテンプレートファイル
;;
;; これらと一致するものがない場合には、デフォルトで "misc" というファ
;; イルが使用されます。
;;
;; 次のような拡張子を使用している場合、
;;
;;   c/h    C 言語
;;   pl     Perl
;;   el     Emacs-Lisp
;;   awk    AWK
;;   sh     Shell Scritp
;;
;; それぞれファイルの拡張子と同じテンプレートファイルが使用されます。
;; 作業ファイルに、決まった拡張子をつけるようにしている場合、その拡張
;; 子と同じファイル名のテンプレートを用意しておけば、そのテンプレート
;; ファイルが使用されます。
;;
;; 拡張子がない場合には、現在編集しているバッファのモードから情報を取
;; り出します。自動的にモードを設定するために用意されている 
;; auto-mode-alist より、モードと拡張子の対応をとりだして使用します。
;; 例えば、auto-mode-alist には、デフォルトで
;;
;;  .c      c-mode
;;  .pas    pascal-mode
;;  .tex    TeX-moe
;;  .text   text-mode
;;  .f      fortran-mode
;;  .tcl    tcl-mode
;;
;; などの定義が行なわれています。この定義から text-mode の場合
;; auto-mode-alist で定義されている拡張子と同じ text というテンプレー
;; トファイルが使用されます。
;; 
;; このほかにもいろいろな拡張子とモードの関係が設定されています。この
;; 定義を参照するのは、*scratch* バッファ上で、
;;
;;   auto-mode-alist
;;
;; と入力してみてください。行の最後で C-j を押します。
;;
;; ファイルの拡張子や、バッファのモードからの情報からテンプレートファ
;; イルを探し出しますが、テンプレートファイルが用意されていない場合 
;; misc というテンプレートファイルを使用します。
;;
;; C 言語のように拡張子 c と h を利用する場合や、形式が同じような場合
;; は、シンボリックリンクすればいいでしょう。新しい言語でもすぐ対応で
;; きるのがいいところですね。
;; 
;; 修正部分にヘッダ情報を追加する目的で
;; 
;;   C-u C-c H
;; 
;; と入力するとファイル名の拡張子が .c のファイルなら c-m というテン
;; プレートファイルを参照し展開します。修正用のテンプレートファイルは、
;; テンプレートファイルに対して "-m" を加えたものになります。これらは、
;; 修正部分の記述を簡単に付け加えたいときに使用します。
;;
;; テンプレートの例 C 言語の c または h
;;
;; /*****
;;  * [FILENAME]
;;  * created: [DATE]
;;  * author: [AUTHOR]
;;  *
;;  * $Source$
;;  */
;;
;; #ifndef lint
;; static char *rcsid_[SUFFIX] =
;; "@(#)$Header$";
;; #endif
;;
;;  
;;
;; /*
;;  * $Log$
;;  */
;;
;; ヘッダ部分の自動生成は、プログラミングなどの作業を行なっているとき
;; にとても効果的です。定型の形式を登録することが簡単ですし、実際に使
;; うときも簡単です。共通のテンプレートファイルを使用すれば、チームで
;; 作業する場合、共通の形式でソースコードにヘッダを付け加えることも可
;; 能です。
;;
;; また、ヘッダ以外の利用方法もあります。面倒な定型の記述を利用する言
;; 語などがありますし、フォーマットが定型のものもあります。このような
;; ものに対して定型部分の記述にも利用可能です。
;;
;; 例 HTML, LaTeX
;;

;; ■ お願い
;;
;; もし機能追加などを行ないましたら、ぜひ作者までお知らせください。便
;; 利な機能を多くの人と共有しましょう。
;;
;; 連絡先 E-mail: Tetsuya.WATANABE atmark nifty.com
;;

(setq header-template-dir "~/.emacs.d/conf/header")

(defvar header-item-alist
  '(("[FILENAME]" . buffer-file-name)
    ("[BASENAME]" . (buffer-name (current-buffer)))
    ("[FULLPATH]" . buffer-file-name)
    ("[AUTHOR]" . (header-author))
    ("[DATE]" . (header-date))
    ("[DD]" . (header-date-dd))
    ("[YMD]" . (header-date-ymd))
    ("[YYYYMMDD]" . (header-date-yyyymmdd))
    ("[YYYYMMDDHHMMSS]" . (header-date-yyyymmddhhmmss))
    ("[SUFFIX]" . (header-suffix buffer-file-name)))
  "ヘッダ情報のキーワード置き換え用

`header-item-alist' で、キーワードに対応する動作を記述します。
この変数は、ヘッダのテンプレートファイルで定義されているキーワー
ドと、そのキーワードに対応するアクションを定義します。キーワー
ドは文字列で指定し、アクションは Emacs-Lisp で記述します(eval 
されます)。

デフォルトでは次のキーワードが登録されています。

[FILENAME]  ファイル名 -> full path
[AUTHOR]    作者       -> login name(full name)
[DATE]      日付       -> December 19,1994 Monday 14:23:08
[SUFFIX]    拡張子     -> sample.c なら c

新しいキーワードとアクションを追加するときには、
`header-item-alist' に定義を追加するか修正します。追加の方法は 
GNU Emacs で auto-mode の拡張の設定で行なう `auto-mode-alist' 
への追加と同様です。ここでは、[MODE] と [TIME] というキーワー
ドにそれぞれ (モードの文字列) と 現在の時間を設定しています。

   (setq header-item-alist
        (append
         '(
           (\"[MODE]\" . mode-name)
           (\"[TIME]\" . (current-time-string))
           ) header-item-alist))

注意事項として、実行したキーワードに対して変換がうまく行かない
場合、キーワードは \"\" (空文字列) と置き換えられます。エラーが
発生した場合わかりにくいですが、すべてのモードで特定の 
Emacs-Lisp を動作させるのは、十分なエラー処理が面倒なためです。

mode-name の場合、そのまま -*- [MODE] -*- としては使えないことがあ
ります。注意してください。GNU Emacs のモードは、間にスペースが入り
ません。このため上記の設定の場合、実際の展開が行なわれた後に修正が
が必要な場合があります。")

(defun header (arg)
  "ヘッダをテンプレートファイルにより自動生成するためのもの

テンプレートファイルは、`header-template-dir' で指定されたディレク
トから次の条件で選びだされます。

   1. 編集しているファイルの拡張子と同じ名前のテンプレートファイル
   2. 編集しているファイルが持つモードから、auto-mode-alist で定義
      されている拡張子の情報を利用し、その定義されている拡張子と同
      じ名前のテンプレートファイル

これらと一致するものがない場合には、デフォルトで \"misc\" というファ
イルが使用されます。

テンプレートファイルでは、キーワード記述しておくと、そのキーワード
に対応する置き換えが実施されます。例えば [FILENAME] と記述されてい
る部分は、編集している「ファイル名」に置き換わります。詳しくは 
`header-item-alist' の説明を参照ください。

さらに詳しい情報は header2.el 自身のコメントを参照ください。"
  (interactive "*P")

  ;; テンプレート用のファイルを template-file に設定(full path)
  (let ((template-file nil))
    ;; lists はテンプレート用 *候補* の list
    ;; dir はテンプレート用ディレクトリ(full path)
    (let ((lists (list "misc"))
          (dir (concat (expand-file-name header-template-dir) "/")))

      ;; 現在のモードからテンプレートのファイル名を作れれば作る
      (let ((alist auto-mode-alist)
            (suffix nil))
        (while (and (not suffix) alist)
          (if (eq major-mode (cdr (car alist)))
              (setq suffix (car (car alist))))
          (setq alist (cdr alist)))
        (if (and suffix
                 (string-match "\\.\\(\\w+\\)" suffix))
            (setq lists (cons (substring suffix (match-beginning 1) (match-end 1)) lists))))

      ;; ファイル名の拡張子からテンプレートのファイル名を作れれば作る
      (if (and (not (null buffer-file-name))
               (string-match "\\.\\(\\w+\\)$" buffer-file-name))
          (setq lists (cons (substring buffer-file-name (match-beginning 1) (match-end 1)) lists)))

      ;; テンプレートのファイル名の候補から file-exists-p を確認
      (let ((tmp nil))
        (while (and (not template-file) lists)
          (setq tmp (concat dir (car lists) (if arg "-m")))
          (if (file-exists-p tmp)
              (setq template-file tmp))
          (setq lists (cdr lists))))

      ;; 結局見付からなければデフォルト(misc)
      (if (not template-file)
          (setq template-file (concat dir "misc"))))

    ;; テンプレートファイルがあった場合(なければ何もしない)
    (if (file-exists-p template-file)
        (let ((tmpbuf (get-buffer-create "*HEADER*"))
              (buffer (current-buffer)))
          (save-excursion
            (buffer-disable-undo tmpbuf)
            (set-buffer tmpbuf)
            (insert-file-contents template-file)
            ;; 置き換え
            (let ((alist header-item-alist))
              (while alist
                (goto-char (point-max))
                (let ((from (car (car alist)))
                      (to nil))
                  (while (search-backward from nil t)
                    (if (not to)
                        (progn
                          (set-buffer buffer)
                          (setq to (or (eval (cdr (car alist))) ""))
                          (set-buffer tmpbuf)))
                    (replace-string from to)))
                (setq alist (cdr alist))))
            ;; もとのバッファーへコピー
            (set-buffer buffer)
            (insert-buffer tmpbuf))
          (kill-buffer tmpbuf)
          (message "Done.")))))

(defun header-suffix (arg)
  "バッファの suffix を返すだけ"
  (if (and arg (string-match "\\.\\(\\w+\\)$" arg))
      (substring arg (match-beginning 1) (match-end 1))
    ""))

(defun header-author ()
  "ログイン名とフルネーム"
  (concat (user-login-name) "(" (user-full-name) ")"))

(defun header-date ()
  "日付情報"
  (let* ((str (current-time-string))
         (year (substring str 20 24))
         (mon (substring str 4 7))
         (month (cond ((string= mon "Jan") "January")
                      ((string= mon "Feb") "February")
                      ((string= mon "Mar") "March")
                      ((string= mon "Apr") "April")
                      ((string= mon "May") "May")
                      ((string= mon "Jun") "June")
                      ((string= mon "Jul") "July")
                      ((string= mon "Aug") "August")
                      ((string= mon "Sep") "September")
                      ((string= mon "Oct") "October")
                      ((string= mon "Nov") "November")
                      ((string= mon "Dec") "December")))
         (weekday (substring str 0 3))
         (weekdayname (cond ((string= weekday "Sun") "Sunday")
                            ((string= weekday "Mon") "Monday")
                            ((string= weekday "Tue") "Tuesday")
                            ((string= weekday "Wed") "Wednesday")
                            ((string= weekday "Thu") "Thursday")
                            ((string= weekday "Fri") "Friday")
                            ((string= weekday "Sat") "Saturday")))
         (d1 (substring str 8 9))
         (d2 (substring str 9 10))
         (time (substring str 11 19)))
    (if (string= d1 " ")
        (setq d1 "0"))
    (concat month " " d1 d2 "," year " " weekdayname " " time)))

(defun header-date-dd ()
  "日付情報"
  (let* ((str (current-time-string))
         (d1 (substring str 8 9))
         (d2 (substring str 9 10)))
    (if (string= d1 " ")
        (setq d1 ""))
    (concat d1 d2)))

(defun header-date-ymd ()
  "日付情報"
  (let* ((str (current-time-string))
         (year (substring str 20 24))
         (mon (substring str 4 7))
         (month (cond ((string= mon "Jan") "1")
                      ((string= mon "Feb") "2")
                      ((string= mon "Mar") "3")
                      ((string= mon "Apr") "4")
                      ((string= mon "May") "5")
                      ((string= mon "Jun") "6")
                      ((string= mon "Jul") "7")
                      ((string= mon "Aug") "8")
                      ((string= mon "Sep") "9")
                      ((string= mon "Oct") "10")
                      ((string= mon "Nov") "11")
                      ((string= mon "Dec") "12")))
         (d1 (substring str 8 9))
         (d2 (substring str 9 10))
         (time (substring str 11 19)))
    (if (string= d1 " ")
        (setq d1 ""))
    (concat year "/" month "/" d1 d2)))

(defun header-date-yyyymmdd ()
  "日付情報"
  (let* ((str (current-time-string))
         (year (substring str 20 24))
         (mon (substring str 4 7))
         (month (cond ((string= mon "Jan") "01")
                      ((string= mon "Feb") "02")
                      ((string= mon "Mar") "03")
                      ((string= mon "Apr") "04")
                      ((string= mon "May") "05")
                      ((string= mon "Jun") "06")
                      ((string= mon "Jul") "07")
                      ((string= mon "Aug") "08")
                      ((string= mon "Sep") "09")
                      ((string= mon "Oct") "10")
                      ((string= mon "Nov") "11")
                      ((string= mon "Dec") "12")))
         (d1 (substring str 8 9))
         (d2 (substring str 9 10))
         (time (substring str 11 19)))
    (if (string= d1 " ")
        (setq d1 "0"))
    (concat year "/" month "/" d1 d2)))

(defun header-date-yyyymmddhhmmss ()
  "日付情報"
  (let* ((str (current-time-string))
         (year (substring str 20 24))
         (mon (substring str 4 7))
         (month (cond ((string= mon "Jan") "01")
                      ((string= mon "Feb") "02")
                      ((string= mon "Mar") "03")
                      ((string= mon "Apr") "04")
                      ((string= mon "May") "05")
                      ((string= mon "Jun") "06")
                      ((string= mon "Jul") "07")
                      ((string= mon "Aug") "08")
                      ((string= mon "Sep") "09")
                      ((string= mon "Oct") "10")
                      ((string= mon "Nov") "11")
                      ((string= mon "Dec") "12")))
         (d1 (substring str 8 9))
         (d2 (substring str 9 10))
         (time (substring str 11 19)))
    (if (string= d1 " ")
        (setq d1 "0"))
    (concat year "/" month "/" d1 d2 " " time)))

(defun date-string ()
  "日付の文字列"
  (interactive)
  (insert (header-date)))

(define-key global-map "\C-cH" 'header)
(define-key global-map "\C-cT" 'date-string)