# rusty_script
rusty scripting language

最初は[この本](http://craftinginterpreters.com/)を読みながらやっていたけど、バイトコードあたりからある程度調べながらやるようになっている  
実際に使えるような言語にはならない　と思う

### Usage
```
git clone https://github.com/komugi1211s/rusty_script.git
cd rusty_script
cargo r
```

### Code example
```
simple_fibonacci: fn(number: int) int {
    if number == 0 return 0;
    if number == 1 return 1;
    return simple_fibonacci(number - 1) + simple_fibonacci(number - 2);
}

print simple_fibonacci(15); // print 610
```

```
simple_fizz_buzz: fn(num: int) {
    i := 1;
    while (i < num) {
        if      i % 15 == 0 print "FizzBuzz";
        else if i %  5 == 0 print "Buzz";
        else if i %  3 == 0 print "Fizz";
        else                print i;

        i = i + 1;
    }
}

simple_fizz_buzz(100);
```
