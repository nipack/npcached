erlang 勉強用の習作です。

なんちゃって memcached です。
レプリケーションとか出来るようになってます。

以下、適当説明。

【準備】
 1. コンパイルしましょう。
 2. mnesia の初期化をします。
   - $erl -snama test -set-cookie test
   - > npcached:setup( disk_copies ).
 3. $mv npcached.app.default npcached.app
 4. $vi npcached.app 
   - 設定適宜変えてください。
 5. $mv log.config.default log.config
 6. $vi log.config
   - 設定適宜変えてください。
 7. 完了

【起動方法】
 1. 起動
   - $erl -boot start_sasl -config log +W w -sname test -set-cookie test
   - > application:start(npcached).

by nipack.