#!perl
use 5.030;

my $out;
my $tot = 1;
while (1) {
    my $count = 0;
    my $idx = 1;
    open $out, "-|", "clash";
    while (<$out>) {
        my ($msg) = $_ =~ /msg=\"(.*)\"/;
        if (/level=info/) {
            $count = 0;
            say "\033[36mINFO\033[0m<$tot:$idx> $msg";
        } elsif (/level=warning/) {
            $count += 1;
            say "\033[33mWARN\033[0m<$tot:$idx> $msg";
        } else {
            print;
        }
        last if $count == 10;
        $idx += 1;
    }
    $tot += 1;
}

