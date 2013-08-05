#!/bin/sh
OTP=`erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell | sed -e 's/"//g'`

FLAGS="-n -nn -Wunmatched_returns -Werror_handling -Wunderspecs"
		
case $OTP in
R16*|R15B03*)
FLAGS="$FLAGS -Wrace_conditions" ;;
esac

echo $FLAGS
