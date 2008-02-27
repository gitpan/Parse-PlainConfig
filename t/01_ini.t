# 01_ini.t
#
# Tests for proper loading of the module

use Parse::PlainConfig;

$|++;
print "1..2\n";

my $test = 1;

# 1 load
my $conf = new Parse::PlainConfig;
ref($conf) eq "Parse::PlainConfig" ? print "ok $test\n" :
  print "not ok $test\n";
$test++;

# 2 alternate load
$conf = Parse::PlainConfig->new('PARAM_DELIM' => '=', PADDING => 1);
ref($conf) eq "Parse::PlainConfig" ? print "ok $test\n" : 
  print "not ok $test\n";
$test++;

# end 01_ini.t
