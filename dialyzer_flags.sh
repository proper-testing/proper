#!/bin/sh
OTP=`erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell | sed -e 's/"//g'`

FLAGS="-Wunmatched_returns -Werror_handling -Wunderspecs"
		
case $OTP in
R16*) FLAGS="$FLAGS -Wrace_conditions --statistics" ;;
R15B03) FLAGS="$FLAGS -Wrace_conditions --statistics" ;;
R15B02) FLAGS="$FLAGS --statistics" ;;
esac

echo "$FLAGS"
