まだ作りかけです。

これは何
========
ログる。


インストール
============

NetInstaller から
-----------------
<del>[カフェイン中毒] からどうぞ。</del>

  [カフェイン中毒]: http://bowbow99.sakura.ne.jp/xyzzy/packages.l

設定
====

使い方
======
（2011-04-08 時点の実装）


    (in-package :my-package))
    
    (require "logging")
    
    (defvar *my-logger* (logging:new-logger "*Buffer Name*" :threshold :debug))
    (logging:setup *my-logger*)

以下のマクロが `:my-package` 内に定義される。

- @debug
- @info
- @msg
- @warn
- @error
- @fatal

以下、上記のどのマクロでも共通。

`message` のようにフォーマット文字列とその引数を与えると、フォーマット
文字列を展開して `logging:new-logger` に指定した名前のバッファに出力され
る。

    (@info "こんにちわ、~A" (user-name))
    => nil

    2011-04-08 21:13:26 [ INFO] こんにちわ、bowbow99

ただし、最初の引数が文字列ではない場合には、それぞれの式とその値が出力さ
れる。

    (let ((x 1)
          (y 2))
      (@debug x y (+ x y)))
    => nil

    2011-04-08 21:15:48 [DEBUG] x => 1
                                y => 2
                                (+ x y) => 3




注意点、既知の問題など
======================

バグ報告、質問、要望などは [GitHubIssues] か [@bowbow99] あたりへお願いします。

  [GitHubIssues]: http://github.com/bowbow99/xyzzy.logging/issues
  [@bowbow99]: http://twitter.com/bowbow99
