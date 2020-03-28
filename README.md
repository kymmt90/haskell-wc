# wc

Basically conforms to GNU wc.

```
$ stack build
$ cat example.txt | stack exec wc-exe
      9     461    3079
$ stack exec wc-exe example.txt
      9     461    3079 example.txt
$ stack exec wc-exe example.txt example2.txt
      9     461    3079 example.txt
     30     225    1529 example2.txt
     39     686    4608 total
```
