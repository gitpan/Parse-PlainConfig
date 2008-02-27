use ExtUtils::MakeMaker;

WriteMakefile(
  NAME          => 'Parse::PlainConfig',
  AUTHOR        => 'Arthur Corliss <corliss@digitalmages.com>',
  ABSTRACT      => 'Parser/Generator of human-readable conf files',
  VERSION_FROM  => 'lib/Parse/PlainConfig.pm',
  PREREQ_PM     => {
    Paranoid          => undef,
    Text::ParseWords  => undef,
    Text::Tabs        => undef,
    },
);

exit 0;
