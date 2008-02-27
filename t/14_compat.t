# 14_compat.t
#
# Tests the traditional usage for backwards compatibility

use Parse::PlainConfig;

$|++;
print "1..9\n";

my $test    = 1;
my $conf    = new Parse::PlainConfig;
my $testrc  = "./t/testrc";
my ($val, $val2);
$conf->read($testrc);

# 1 Test purge property set
$conf->purge(1);
$conf->property("AUTOPURGE") ? print "ok $test\n" : 
  print "not ok $test\n";
$test++;

# 2 Test FORCE_SCALAR property
$conf->property("FORCE_SCALAR", ["SCALAR 1", "SCALAR 2"]);
$val = $conf->property("COERCE");
$$val{'SCALAR 1'} eq 'string' ? print "ok $test\n" : print "not ok $test\n";
$test++;

# 3 Test DELIM property
$conf->property("DELIM", "**");
$val = $conf->property("PARAM_DELIM");
$val eq "**" ? print "ok $test\n" : print "not ok $test\n";
$test++;

# 4 Test delim method
$conf->delim("=");
$val = $conf->property("PARAM_DELIM");
$val eq "=" ? print "ok $test\n" : print "not ok $test\n";
$test++;

# 5 Test get method
$val = $conf->get('SCALAR 1');
$val eq "value1" ? print "ok $test\n" : print "not ok $test\n";
$test++;

# 6 Test get method again
($val, $val2) = $conf->get('SCALAR 1', 'SCALAR 3');
$val2 eq "this is a continued line." ? print "ok $test\n" : 
  print "not ok $test\n";
$test++;

# 7 Test set method
$conf->set('SCALAR 1', 'value one');
$val = $conf->get('SCALAR 1');
$val eq 'value one' ? print "ok $test\n" : print "not ok $test\n";
$test++;

# 8 Test get_ref method
$val = $conf->get_ref;
$val2 = $$val{'SCALAR 1'};
$val2 eq 'value one' ? print "ok $test\n" : print "not ok $test\n";
$test++;

# 9 Test error method
Parse::PlainConfig::ERROR = 'ouch!';
$val = $conf->error;
$val eq 'ouch!' ? print "ok $test\n" : print "not ok $test\n";
$test++;

# end 14_compat.t
