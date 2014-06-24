#!/bin/sh
erl +A 5 +K true -name tcp_failing_node -setcookie dev -boot start_sasl -pa ./ebin -pa -s tcp_failing_ex_app
