# 04_parameters.t
#
# Tests the parameters method

use Parse::PlainConfig;

$|++;
print "1..7\n";

my $test   = 1;
my $testrc = "./t/testrc";
my $conf   = Parse::PlainConfig->new(FILE => $testrc);
my @test   = ("SCALAR 1", "SCALAR 2", "SCALAR 3", "LIST 1", "LIST 2",
              "HASH 1");
my @params;

# 1 Make sure parameters have been read
$conf->read;
@params = $conf->parameters;
scalar @params > 1 ? print "ok $test\n" : print "not ok $test\n";
$test++;

# 2 .. 7 Make sure specific parameters are present
foreach (@test) {
  grep(/^\Q$_\E$/, @params) ? print "ok $test\n" : 
    print "not ok $test\n";
  $test++;
}

# end 04_parameters.t
