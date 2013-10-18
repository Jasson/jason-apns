erlang server for APNS depend inaka/apns4erl 
=====

EXAMPLE
=====



 ./rebar get-deps
 ./rebar compile
 
  erl -pa ebin/ deps/*/ebin/
  application:start(crypto). 
  application:start(public_key).
  application:start(ssl). 
  application:start(apns).  
  application:start(eapns).



 send test  message
 trends_apns:send_with_extra(["6654f94dfe37626c2242fb2d0d23719fe8168ab3719b750d40497eb356349c0b"],"s1",<<"{\"mt\":\"3\",\"st\":\"2\",\"et\":\"active-1380530760163932\"}">>).
