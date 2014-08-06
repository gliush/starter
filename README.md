### install
    make
### examples
    1> starter_pool:evaluate(python, "print 1;").
    {ok,[{stdout,[<<"1\n">>]}]}

    2> starter_pool:evaluate(ruby, "puts 1; put 1").
    {error,[{exit_status,256},
            {stdout,[<<"1\n">>]},
            {stderr,[<<"/var/tmp/starter/e0edcf4c-09df-11e4-9385-0800270c413e:1:in `<main>'">>,
                     <<": ">>,<<"undefined method `put' for main:Object">>,
                     <<" (">>,<<"NoMethodError">>,<<")\n">>]}]}

    3> starter_pool:evaluate(javascript, "console.log(1);").
    {ok,[{stdout,[<<"1\n">>]}]}

    It's also possible to pass files:
    4> base64:encode("a=3").
    <<"YT0z">>

    $ curl --data '{"language":"bash","code":"source aaa; echo $a","inputFiles":{"aaa":"YT0z"}}' 127.0.0.1:8080/api/evaluate
    {"stdout":["3\n"]}
