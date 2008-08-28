# Parse::PlainConfig -- Parser for plain-text configuration files
#
# (c) 2002 - 2006, Arthur Corliss <corliss@digitalmages.com>,
#
# $Id: PlainConfig.pm,v 2.06 2008/07/07 22:59:35 acorliss Exp $
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#####################################################################

=head1 NAME

Parse::PlainConfig - Parser for plain-text configuration files

=head1 MODULE VERSION

$Id: PlainConfig.pm,v 2.06 2008/07/07 22:59:35 acorliss Exp $

=head1 SYNOPSIS

  use Parse::PlainConfig;

  $conf = new Parse::PlainConfig;
  $conf = Parse::PlainConfig->new(
    'PARAM_DELIM' => '=',
    'FILE'        => '.myrc',
    'MAX_BYTES'   => 65536,
    'SMART_PARSER => 1,
    );

  $conf->property(PARAM_DELIM => '=');

  $rv = $conf->read('myconf.conf');
  $rv = $conf->read;
  $rv = $conf->readIfNewer;
  $conf->write('.myrc', 2);

  $conf->purge;

  @parameters = $conf->parameters;
  $conf->parameter(FOO => "bar");
  $value = $conf->parameter(FOO);
  $conf->describe(FOO => 'This is foo');
  $conf->coerce("string", qw(FOO BAR));

  @order = $conf->order;
  $conf->order(@new_order);

  $errstr = Parse::PlainConfig::ERROR;

  $rv = $conf->hasParameter('FOO');

The following methods are only supported for backwards compatibility reasons.
They will likely be removed at some point in the future.

  # Use of the tags DELIM and PURGE are deprecated in favour of
  # PARAM_DELIM, LIST_DELIM, HASH_DELIM, and AUTOPURGE
  $conf = Parse::PlainConfig->new(
    'DELIM' => '=',
    'PURGE' => 1,
    );

  # As is the delim method since it works only on the tag delimiter
  $conf->delim('=');

  # Auto-purge should be enabled/disabled via the property method
  $conf->purge(1);

  # directives is replaced with parameters
  @directives = $conf->directives;

  # get/set methods are replaced with a unifed parameter method
  $field = $conf->get('KEY1');
  ($field1, $field2) = $conf->get(qw(KEY1 KEY2));
  $conf->set(KEY1 => 'foo', KEY2 => 'bar');

  # This was just a really bad idea to begin with, plus it's 
  # effective broken at this point (only returns a copy of the
  # internal hash now, so it's effectively read-only)
  $hashref = $conf->getRef;

  # This is just a wrapper for the class function
  $errstr = $conf->error

=head1 REQUIREMENTS

=over

=item *

Paranoid

=item *

Text::ParseWords

=item *

Text::Tabs

=back

=head1 DESCRIPTION

Parse::PlainConfig provides OO objects which can parse and generate
human-readable configuration files.

=cut

#####################################################################
#
# Environment definitions
#
#####################################################################

package Parse::PlainConfig;

use strict;
use vars qw($VERSION);
use Text::ParseWords;
use Text::Tabs;
use Carp;
use Fcntl qw(:flock);
use Paranoid::Debug;
use Paranoid::Filesystem;
use Paranoid::Input;

($VERSION) = (q$Revision: 2.06 $ =~ /(\d+(?:\.(\d+))+)/);

#####################################################################
#
# Module code follows
#
#####################################################################

=head1 FILE SYNTAX

=head2 TRADITIONAL USAGE

The plain parser supports the reconstructions of relatively simple data
structures.  Simple string assignments and one-dimensional arrays and hashes
are possible.  Below are are various examples of constructs:

  # Scalar assignment
  FIRST_NAME: Joe
  LAST_NAME: Blow

  # Array assignment
  FAVOURITE_COLOURS: red, yellow, green
  ACCOUNT_NUMBERS:  9956-234-9943211, \
                    2343232-421231445, \
                    004422-03430-0343
  
  # Hash assignment
  CARS:  crown_vic => 1982, \
         geo       => 1993

As the example above demonstrates, all lines that begin with a '#' (leading
whitespace is allowed) are ignored as comments.  if '#" occurs in any other
position, it is accepted as part of the passed value.  This means that you
B<cannot> place comments on the same lines as values.

All directives and associated values will have both leading and trailing 
whitespace stripped from them before being stored in the configuration hash.  
Whitespace is allowed within both.

In traditional mode (meaning no parameters are set to be coerced into a
specific datatype) one must encapsulate list and hash delimiters with
quotation marks in order to prevent the string from being split and stored as
a list or hash.  Quotation marks that are a literal part of the string must be
backslashed.

=head2 SMART PARSER

The new parser now provides some options to make the file syntax more
convenient.  You can activate the smart parser by setting B<SMART_PARSER> to a
true value during object instantiation or via the B<property> method.

With the traditional parser you had to backslach the end of all preceding
lines if you wanted to split a value into more than one line:

  FOO:  This line starts here \
        and ends here...

With the smart parser enabled that is no longer necessary as long as the
following lines are indented further than the first line:

  FOO:  This line starts here
        and ends here...

B<Note:>  The indentation is compared by byte count with no recognition of
tab stops.  That means if you indent with spaces on the first line and indent
with tabs on the following it may not concantenate those values.

Another benefit of the smart parser is found when you specify a parameter to
be of a specific datatype via the B<COERCE> hash during object instantiation
or the B<coerce> method.  For instance, the traditional parser requires you to
encapsulate strings with quotation marks if they contain list or hash
delimiters:

  Quote:  "\"It can't be that easy,\" he said."

Also note how you had to escape quotation marks if they were to be a literal
part of the string.  With this parameter set to be coerced to a scalar you can
simply write:

  Quote:  "It can't be that easy," he said.

Similarly, you don't have to quote hash delimiters in parameters set to be
coerced into lists.  Quotation marks as part of an element value must be
escaped, though, since unescaped quotation marks are assumed to encapsulate
strings containing list delimiters you don't want to split on.

B<Note:> The previous versions of Parse::PlainConfig did not allow the user to
set keys like:

  FOO: \
      bar

or save empty assignments like

  FOO:

This is no longer the case.  Both are now valid and honoured.

=head1 SECURITY

B<WARNING:> This parser will attempt to open what ever you pass to it for a
filename as is.  If this object is to be used in programs that run with
permissions other than the calling user, make sure you sanitize any
user-supplied filename strings before passing them to this object.

This also uses a blocking b<flock> call to open the file for reading and
writing.

=head1 FUNCTIONS

=head2 Parse::PlainConfig::ERROR

=cut

{
  my $ERROR = '';

  sub ERROR : lvalue { $ERROR };
}

=head1 METHODS

=head2 new

  $conf = new Parse::PlainConfig;
  $conf = Parse::PlainConfig->new(
    'PARAM_DELIM' => '=',
    'FILE'        => '.myrc',
    'MAX_BYTES'   => 65536,
    'SMART_PARSER => 1,
    );

The object constructor can be called with or without arguments.  Arguments
available for use include:

  Argument        Default    Purpose
  =============================================================
  ORDER           []         Specifies specific order of
                             fields to be used while writing
  FILE            undef      Filename for read/write ops
  PARAM_DELIM       ':'        Field/value delimiter
  LIST_DELIM      ','        List delimiter within field values
  HASH_DELIM      '=>'       Hash key/value delimiter within
                             field values
  AUTOPURGE       0          Autopurge enabled/disabled
  COERCE          {}         Field coercion hash
  DEFAULTS        {}         Default field values
  SMART_PARSER    0          Smart parser enabled/disabled
  MAX_BYTES       16384      Integer denoting maximum bytes
                             to read in any given file

B<DELIM>, B<PURGE>, and  B<FORCE_SCALAR> are still available for backwards 
compatibility, but may be removed in the future.  One should use 
B<PARAM_DELIM> B<AUTOPURGE>, and B<COERCE> instead.

B<COERCE> is a hash of field name/data type pairs.  If a field is listed in
this hash then their values will always be returned in the requested format of
either string, list, or hash.  Any field coerced to string, for instance, will
ignore list and hash delimiters and assume the entire value will always be
string value.

B<DEFAULTS> is a hash of field name/value pairs.  This ensures that even if a
field is not explicitly set (either in a conf file or programmatically) a
default value can still be retrieved.

B<SMART_PARSER> removes the need to backslash end-of-lines to continue the
value onto the next.  If the following line is indented further than the tag
was it will automatically assume that the next line is a continuation of the
previous.  It also affects the need to encapsulate coerced datatypes with
quotation marks for irrelevant delimiters.

B<AUTOPURGE> erases all stored parameters and values before reading a file.
This does not, however, erase any values set for B<ORDER>.

=cut

sub new {
  my $class = shift;
  my %init = (
    CONF          => {},
    ORDER         => [],
    FILE          => undef,
    PARAM_DELIM   => ':',
    LIST_DELIM    => ',',
    HASH_DELIM    => '=>',
    AUTOPURGE     => 0,
    COERCE        => {},
    DEFAULTS      => {},
    SMART_PARSER  => 0,
    PADDING       => 2,
    MAX_BYTES     => 16384,
    MTIME         => 0,
    );
  my $self = \%init;
  my %args = @_;
  my (@keyList, $k, $v, $rv);

  pdebug("entering", 7);
  pIn();

  bless $self, $class;

  # Assign all the arguments
  $rv = 1;
  while ($rv && scalar keys %args) {
    $k = shift @{[ keys %args ]};
    $v = $args{$k};
    delete $args{$k};
    $rv = $self->property($k, $v);
  }

  # Return the object reference if no errors occurred during initialization
  if ($rv) {
    $v = $rv = $self;
  } else {
    $rv = undef;
    $v = 'undef';
  }

  pOut();
  pdebug("leaving w/rv: $v", 7);

  return $self;
}

=head2 property

  $conf->property(PARAM_DELIM => '=');

This method sets or retrieves the specified property.  Please note
that this B<overwrites> the current value, even for those properties that are
references to lists and hashes.

If you're using this to set a property it will return a boolean true or false
depending on the success of the operation.  If you're just retrieving a
property it will return the value of the property.  If you ask for a
nonexistent property it will B<croak>.

=cut

sub property ($$;$) {
  my $self  = shift;
  my @args  = @_;
  my $arg   = $args[0];
  my $val   = $args[1];
  my $ival  = defined $val ? $val : 'undef';
  my $rv    = 1;

  croak "Parse::PlainConfig::property was called with an undefined property"
    unless defined $arg;
  $arg = 'PARAM_DELIM' if $arg eq 'DELIM';
  $arg = 'AUTOPURGE'   if $arg eq 'PURGE';
  croak "Parse::PlainConfig::property was called with an unknown property" .
    "($arg)" unless exists $$self{$arg} or $arg eq 'FORCE_SCALAR';

  pdebug("entering w/($arg)($ival)", 7);
  pIn();

  pdebug("method is in " . (scalar @args == 2 ? 'set' : 'get') . " mode", 7);
  $arg = uc($arg);

  # TODO 2008/05/11: properties deprecated, remove FORCE_SCALAR, DELIM, PURGE

  # Validate argument & value
  if (scalar @args == 2) {

    # Make sure list properties are list references
    if ($arg =~ /^(?:ORDER|FORCE_SCALAR)$/) {
      unless (ref($val) eq 'ARRAY') {
        $rv = 0;
        ERROR = pdebug("${arg}'s value must be a list reference", 7);
      }

    # Hash properties are hash references
    } elsif ($arg =~ /^(?:CONF|COERCE|DEFAULTS)$/) {
      unless (ref($val) eq 'HASH') {
        $rv = 0;
        ERROR = pdebug("${arg}'s value must be a hash reference", 7);
      }

      # Validate coerced values
      if ($rv && $arg eq 'COERCE') {
        foreach (keys %$val) {
          $ival = defined $$val{$_} ? $$val{$_} : 'undef';
          ERROR = pdebug("coerced data type ($_: $ival) not a string, " .
            "list, or hash") and $rv = 0 unless 
            $ival =~ /^(?:string|list|hash)$/;
        }
      }

    # And the rest are scalars...
    # TODO?  Validate properties like PADDING that have a concrete list of 
    # TODO?  valid values?
    } elsif (ref($val) ne '') {
      $rv = 0;
      ERROR = pdebug("${arg}'s value must be a scalar value", 7);
    }
  }

  # Set the value if all's kosher
  if ($rv) {
    if (scalar @args == 2) {
      if ($arg eq 'FORCE_SCALAR') {
        foreach (@$val) {  $$self{COERCE}{$_} = 'string' };
      } else {
        $$self{$arg} = $val;
      }
    } else {
      $rv = $$self{$arg};
    }
  }

  pOut();
  pdebug("leaving w/rv: $rv", 7);

  return $rv;
}

=head2 purge

  $conf->purge(1);
  $conf->purge;

B<NOTE:>  Use of this method to set the purge mode is deprecated and will be
removed in the future.  For that please use the B<property> method instead.

This method either (re)sets the auto-purge mode, or performs an immediate manual
purge.  Auto-purge mode clears the configuration hash each time a
configuration file is read, so that the internal configuration data consists
solely of what is in that file.  If you wanted to combine the settings of
multiple files that each may exclusively hold some directives, setting this to
'off' will load the combined configuration as you read each file.

You can still clobber configuration values, of course, if the same directive
is defined in multiple files.  In that case, the last file's value will be the
one stored in the hash.

This does not clear the B<order> or B<coerce> properties.

Autopurge mode is disabled by default.

=cut

sub purge($$) {
  my $self  = shift;
  my $arg   = shift;

  $arg = 'undef' unless defined $arg;
  pdebug("entering w/($arg)", 7);
  pIn();

  # TODO: 2008/05/11: property set invocation deprecated, remove

  if ($arg ne 'undef') {
    pdebug("setting AUTOPURGE to $arg", 7);
    $self->property('AUTOPURGE', $arg);
  } else {
    pdebug("clearing CONF", 7);
    $$self{CONF}  = {};
  }

  pOut();
  pdebug("leaving w/rv: 1", 7);

  return 1;
}

=head2 read

  $rv = $conf->read('myconf.conf');
  $rv = $conf->read;

The read method is called initially with a filename as the only argument.
This causes the parser to read the file and extract all of the configuration
directives from it.

You'll notice that you can also call the read method without an argument.
The name of the file read is stored internally, and if already set to a valid
value (either by a previous call to B<read> with a filename argument or by
setting the B<FILE> property) this will read that file's contents.

The return value will be one if the file was successfully read and parsed, 
or zero otherwise.  The reason for failure can be read via
B<Parse::PlainConfig::ERROR>.

This function will cause the program to croak if called without a filename 
ever being defined.

=cut

sub read($;$) {
  my $self    = shift;
  my $file    = shift || $$self{FILE};
  my $purge   = $$self{AUTOPURGE};
  my $rv      = 0;
  my $oldSize = FSZLIMIT;
  my ($line, @lines);

  croak "Parse::PlainConfig::read called an undefined filename" unless
    defined $file;

  pdebug("entering w/($file)", 7);
  pIn();

  # Reset the error string and update the internal filename
  ERROR = '';
  $$self{FILE}  = $file;

  # Temporarily set the specified size limit
  FSZLIMIT = $$self{MAX_BYTES};

  # Store the file's current mtime
  $$self{MTIME} = (stat $file)[9];

  if (slurp($file, \@lines, 1)) {

    # Empty the current config hash and key order
    $self->purge if $purge;

    # Parse the rc file's lines
    $rv = $self->_parse(@lines);

  } else {
    ERROR = Paranoid::ERROR;
  }

  # Restore old size limit
  FSZLIMIT = $oldSize;

  pOut();
  pdebug("leaving w/rv: $rv", 7);

  # Return the result code
  return $rv;
}

=head2 readIfNewer

  $rv = $conf->readIfNewer;

This method is used to reread & parse the file only if the mtime appears
newer than when last read.  If the file was successfully reread or appears to
be the same it will return true.  Any errors will be stored in
B<Parse::PlainConfig::ERROR> and it will return a false value.

You can determine whether or not the file was read by the true value.  If it
was re-read it will return 1.  If the file appears to be the same age it will 
return a 2.

=cut

sub readIfNewer($) {
  my $self    = shift;
  my $file    = $$self{FILE};
  my $omtime  = $$self{MTIME};
  my $rv      = 0;
  my $mtime;

  croak "Parse::PlainConfig::readIfNewer called an undefined filename" unless
    defined $file;

  pdebug("entering w/($file)", 7);
  pIn();

  # Make sure the file exists and is readable
  if (-e $file && -r _) {

    # Read if the file appears to be newer
    $mtime = (stat _)[9];
    pdebug("current mtime: $mtime last: $omtime", 7);
    $rv = $mtime > $omtime ? $self->read : 2;

  # Report errors
  } else {
    ERROR = "Parse::PlainConfig::readIfNewere: File ($file) does not exist " .
      "or is not readable!";
  }

  pOut();
  pdebug("leaving w/rv: $rv", 7);

  # Return the result code
  return $rv;
}

=head2 write

  $conf->write('.myrc', 2);

This method writes the current configuration stored in memory to the specified
file, either specified as the first argument, or as stored from an explicit or
implicit B<read> call.

The second argument specifies what kind of whitespace padding, if any, to use
with the directive/value delimiter.  The following values are recognised:

  Value    Meaning
  ================================================
  0        No padding (i.e., written as KEY:VALUE)
  1        Left padding (i.e., written as KEY :VALUE)
  2        Right padding (i.e., written as KEY: VALUE)
  3        Full padding (i.e., written as KEY : VALUE)

Both arguments are optional.

=cut

sub write($;$$) {
  my $self        = shift;
  my $file        = shift || $self->{FILE};
  my $padding     = shift;
  my $conf        = $self->{CONF};
  my $order       = $self->{ORDER};
  my $coerce      = $self->{COERCE};
  my $smart       = $self->{SMART_PARSER};
  my $paramDelim  = $self->{PARAM_DELIM};
  my $hashDelim   = $self->{HASH_DELIM};
  my $listDelim   = $self->{LIST_DELIM};
  my $rv          = 0;
  my $tw          = 78;
  my $delimRegex  = qr/(?:\Q$hashDelim\E|\Q$listDelim\E)/;
  my (@forder, $type, $param, $value, $description, $entry, $out);
  my ($tmp, $tvalue, $lines);

  # TODO: Implement non-blocking flock support
  # TODO: Store read padding and/or use PADDING property value

  croak "Parse::PlainConfig::write called an undefined filename" unless
    defined $file;
  $padding = 2 unless defined $padding;
  $tw -= 2 unless $smart;

  pdebug("entering w/($file)($padding)", 7);
  pIn();

  # Pad the delimiter as specified
  $paramDelim = $padding == 0 ? $paramDelim : $padding == 1 ? " $paramDelim" : 
    $padding == 2 ? "$paramDelim " : " $paramDelim ";
  pdebug("PARAM_DELIM w/padding is '$paramDelim'", 
    7);

  # Create a list of parameters for output
  @forder = @$order;
  foreach $tmp (sort keys %$conf) { push (@forder, $tmp) unless 
    grep /^\Q$tmp\E$/, @forder };
  pdebug("order of params to be " .
    "written:\n\t@forder", 7);

  # Compose the new output
  $out = '';
  foreach $param (@forder) {

    # Determine the datatype
    $value        = exists $$conf{$param} ? $$conf{$param}{Value} : '';
    $description  = exists $$conf{$param} ? $$conf{$param}{Description} : '';
    $type         = exists $$coerce{$param} ? $$coerce{$param} : 
      ref($value) eq 'HASH' ?  'hash' : ref($value) eq 'ARRAY' ?  
      'list' : 'string';
    pdebug("adding $type param ($param)", 7);

    # Append the comments
    $out .= $description;
    $out .= "\n" unless $out =~ /\n$/m;

    # Start the new entry with the parameter name and delimiter
    $entry = "$param$paramDelim";

    # Append the value, taking into consideration the smart parser
    # and coercion settings
    if ($type eq 'string') {
      $tvalue = $value;
      unless ($smart && exists $$coerce{$param}) {
        $tvalue =~ s/"/\\"/g;
        $tvalue = "\"$tvalue\"" if $tvalue =~ /$delimRegex/;
      }
      $lines = "$entry$tvalue";
    } elsif ($type eq 'list') {
      $tvalue = [ @$value ];
      foreach (@$tvalue) {
        s/"/\\"/g;
        if ($smart && exists $$coerce{$param}) {
          $_ = "\"$_\"" if /\Q$listDelim\E/;
        } else {
          $_ = "\"$_\"" if /$delimRegex/;
        }
      }
      $lines = $entry . join(" $listDelim ", @$tvalue);
    } else {
      $tvalue = { %$value };
      foreach (keys %$tvalue) {
        $tmp = $_;
        $tmp =~ s/"/\\"/g;
        $tmp = "\"$tmp\"" if /$delimRegex/;
        if ($tmp ne $_) {
          $$tvalue{$tmp} = $$tvalue{$_};
          delete $$tvalue{$_};
        }
        $$tvalue{$tmp} =~ s/"/\\"/g;
        $$tvalue{$tmp} = "\"$$tvalue{$tmp}\"" if 
          $$tvalue{$tmp} =~ /$delimRegex/;
      }
      $lines = $entry . join(" $listDelim ", 
        map { "$_ $hashDelim $$tvalue{$_}" } sort keys %$tvalue);
    }

    # wrap the output to the column width and append to the output
    $out .= _wrap("", "\t", $tw, ($smart ? "\n" : "\\\n"), $lines);
    $out .= "\n" unless $out =~ /\n$/m;
  }

  # Attempt to open the file
  if (detaint($file, 'filename', \$file)) {
    if (open(RCFILE, "> $file")) {

      # Write the file
      flock(RCFILE, LOCK_EX);
      if (print RCFILE $out) {
        $rv = 1;
      } else {
        ERROR = $!;
      }
      flock(RCFILE, LOCK_UN);
      close(RCFILE);

      # Store the new mtime on successful writes
      $$self{MTIME} = (stat $file)[9] if $rv;

    # Opening the file failed
    } else {
      ERROR = "Parse::PlainConfig::write: Error writing file: $!";
    }

  # Detainting filename failed
  } else {
    ERROR = "Parse::PlainConfig::write: illegal characters in filename: " .
      $file;
  }

  pOut();
  pdebug("leaving w/rv: $rv", 7);

  return $rv;
}

=head2 parameters

  @parameters = $conf->parameters;

This method returns a list of all the names of the directives currently 
stored in the configuration hash in no particular order.

=cut

sub parameters() {
  my $self        = shift;
  my @parameters  = keys %{ $$self{CONF} };

  pdebug("Called Parse::PlainConfig::parameters -- rv: @parameters", 7);

  return @parameters;
}

=head2 parameter

  $value = $conf->parameter('SCALAR1');
  @values = $conf->parameter('LIST1');
  %values = $conf->parameter('HASH1');
  $conf->parameter('SCALAR1', "foo");
  $conf->parameter('LIST1', [qw(foo bar)]);
  $conf->parameter('HASH1', { foo => 'bar' });

This method sets or retrieves the specified parameter.  Hash and list values
are copied and returned as a list.  If the specified parameter is set to be
coerced into a specific data type the specified value will be converted to
that datatype.   This means you can do something like:

  # SCALAR1 will equal "foo , bar , roo" assuming LIST_DELIM is set to ','
  $conf->coerce(qw(string SCALAR1));
  $conf->parameter('SCALAR1', [qw(foo bar roo)]);

  # SCALAR1 will equal "foo => bar : roo => ''" assuming HASH_DELIM is set
  # to '=>' and LIST_DELIM is set to ':'
  $conf->parameter('SCALAR1', { 'foo' => 'bar', 'roo' => '' });

In order for conversions to be somewhat predictable (in the case of hashes
coerced into other values) hash key/value pairs will be assigned to string
or list portions according to the alphabetic sort order of the keys.

=cut

sub parameter($$;$) {
  my $self          = shift;
  my @args          = @_;
  my $param         = $args[0];
  my $value         = $args[1];
  my $ivalue        = defined $value ? $value : 'undef';
  my $conf          = $$self{CONF};
  my $listDelim     = $$self{LIST_DELIM};
  my $hashDelim     = $$self{HASH_DELIM};
  my $paramDelim    = $$self{PARAM_DELIM};
  my $coerceType    = exists $$self{COERCE}{$param} ? $$self{COERCE}{$param} :
                      'undef';
  my $defaults      = $$self{DEFAULTS};
  my $rv            = 1;
  my ($finalValue, @elements);

  # TODO: Consider storing a list/hash padding value as well, for use
  # TODO: in coercion to string.

  croak "Parse::PlainConfig::parameter was called with an undefined parameter"
    unless defined $param;

  pdebug("entering w/($param)($ivalue)", 7);
  pIn();

  if (scalar @args == 2) {
    pdebug("method in set mode", 7);

    # Create a blank record if it hasn't been defined yet
    $$conf{$param} = {
      Value       => '',
      Description => '',
      } unless exists $$conf{$param};

    # Start processing value assignment
    if ($coerceType ne 'undef') {
      pdebug("coercing into $coerceType", 7);

      # Coerce values into strings
      if ($coerceType eq 'string' && ref($value) ne '') {

        # Convert lists into a string using the list delimiter
        if (ref($value) eq 'ARRAY') {
          foreach (@$value) {
            s/"/\\"/g;
            $_ = "\"$_\"" if /\Q$listDelim\E/;
          }
          $finalValue = join(" $listDelim ", @$value);

        # Convert hashes into a string using the hash & list delimiters
        } elsif (ref($value) eq 'HASH') {
          foreach (sort keys %$value) {
            $ivalue     = $_;
            $ivalue     =~ s/"/\\"/g;
            $ivalue     = "\"$ivalue\"" if /(?:\Q$hashDelim\E|\Q$listDelim\E)/;
            $$value{$_} = '' unless defined $$value{$_};
            $$value{$_} = "\"$$value{$_}\"" if 
              $$value{$_} =~ /(?:\Q$hashDelim\E|\Q$listDelim\E)/;
            push(@elements, join(" $hashDelim ", $_, (defined $$value{$_} ?
              $$value{$_} : ''))) };
            $finalValue = join(" $listDelim ", @elements);

        # Try to stringify everything else
        } else {
          $finalValue = "$value";
        }

      # Coerce value into a list
      } elsif ($coerceType eq 'list' && ref($value) ne 'ARRAY') {

        # Convert hashes into a list
        if (ref($value) eq 'HASH') {
          $finalValue = [];
          foreach (sort keys %$value) { push(@$finalValue, $_, $$value{$_}) };

        # Convert strings into a list
        } elsif (ref($value) eq '') {
          $self->_parse(split(/\n/m, 
            "$$conf{$param}{Description}\n$param $paramDelim $value"));
          $finalValue = $$conf{$param}{Value};

        # Stringify everything else and put it into an array
        } else {
          $finalValue = [ "$value" ];
        }

      # Coerce value into a hash
      } elsif ($coerceType eq 'hash' && ref($value) ne 'HASH') {

        # Convert a list into a hash using every two elements as a 
        # key/value pair
        if (ref($value) eq 'ARRAY') {
          push(@$value, '') unless int(scalar @$value / 2) == 
            scalar @$value / 2;
          $finalValue = { @$value };

        # Convert strings into a hash
        } elsif (ref($value) eq '') {
          $self->_parse(split(/\n/m, 
            "$$conf{$param}{Description}\n$param $paramDelim $value"));
          $finalValue = $$conf{$param}{Value};

        # Stringify everything else and put the value into the hash key
        } else {
          $finalValue = { "$value" => '' };
        }

      # No coercion is necessary
      } else {
        $finalValue = $value;
      }

    } else {
      pdebug("no coercion to do", 7);
      $finalValue = $value;
    }
    $$conf{$param}{Value} = $finalValue;

  } else {
    pdebug("method in retrieve mode", 7);
    $rv = exists $$conf{$param} ? $$conf{$param}{Value} : 
      exists $$defaults{$param} ? $$defaults{$param} :
      undef;
  }

  pOut();
  pdebug("leaving w/rv: " . (defined $rv ? $rv : 'undef'), 7);

  return ref($rv) eq 'HASH' ? (%$rv) : ref($rv) eq 'ARRAY' ? (@$rv) : $rv;
}

=head2 coerce

  $conf->coerce("string", "FOO", "BAR");

This method configures the parser to coerce values into the specified
datatype (either string, list, or hash) and immediately convert any existing
values and store them into that datatype as well.

=cut

sub coerce($$@) {
  my $self    = shift;
  my $type    = shift;
  my $itype   = defined $type ? $type : 'undef';
  my @params  = @_;

  croak "Parse::PlainConfig::coerce called with an invalid datatype ($itype)"
    unless $itype =~ /^(?:string|list|hash)$/;
  croak "Parse::PlainConfig::coerce called with no named parameters" unless
    @params;

  pdebug("entering w/($itype)(@params)", 7);
  pIn();

  foreach (@params) {
    $$self{COERCE}{$_} = $type;
    $self->parameter($_, $$self{CONF}{$_}{Value}) if exists $$self{CONF}{$_};
  }

  pOut();
  pdebug("leaving w/rv: 1", 7);
}

=head2 describe

  $conf->describe(KEY1 => 'This is foo', KEY2 => 'This is bar');

The describe method takes any number of key/description pairs which will be
used as comments preceding the directives in any newly written conf file.  You
are responsible for prepending a comment character to each line, as well as
splitting along your desired text width.

=cut

sub describe($@) {
  my $self    = shift;
  my $conf    = $$self{CONF};
  my $coerce  = $$self{COERCE};
  my %new     = (@_);

  pdebug("entering", 7);
  pIn();

  # TODO: Consider allowing comment tags to be specified

  # TODO: Consider line splitting and comment tag prepending where
  # TODO: it's not already done.

  foreach (keys %new) {
    pdebug("$_ is described as '$new{$_}'", 7);
    unless (exists $$conf{$_}) {
      $$conf{$_} = {};
      if (exists $$coerce{$_}) {
        $$conf{$_}{Value} = $$coerce{$_} eq 'list' ? [] :
          $$coerce{$_} eq 'hash' ? {} : '';
      } else {
        $$conf{$_}{Value} = '';
      }
    }
    $$conf{$_}{Description} = $new{$_};
  }

  pOut();
  pdebug("leaving w/rv: 1", 7);

  return 1;
}

=head2 order

  @order = $conf->order;
  $conf->order(@new_order);

This method returns the current order of the configuration directives as read 
from the file.   If called with a list as an argument, it will set the
directive order with that list.  This method is probably of limited use except 
when you wish to control the order in which directives are written in new conf 
files.

Please note that if there are more directives than are present in this list, 
those extra keys will still be included in the new file, but will appear in
alphabetically sorted order at the end, after all of the keys present in the
list.

=cut

sub order($@) {
  my $self  = shift;
  my $order = $$self{ORDER};
  my @new   = (@_);

  pdebug("entering w/(@new)", 7);

  @$order = (@new) if scalar @new;

  pdebug("leaving w/rv: @$order", 7);

  return @$order;
}


sub _parse($@) {
  # Parses the passed list of lines and extracts comments, fields, and
  # values from them, storing them in the CONF hash.
  #
  # Usage:  $self->_parse(@lines);

  my $self      = shift;
  my $conf      = $$self{CONF};
  my $order     = $$self{ORDER};
  my $smart     = $$self{SMART_PARSER};
  my $tagDelim  = $$self{PARAM_DELIM};
  my $hashDelim = $$self{HASH_DELIM};
  my $listDelim = $$self{LIST_DELIM};
  my @lines     = @_;
  my $rv        = 1;
  my ($i, $line, $comment, $entry, $field, $value);
  my ($indentation, $data);

  # Make sure some of the properties are sane
  croak "LIST_DELIM and HASH_DELIM cannot be the same character sequence!\n"
    unless $$self{LIST_DELIM} ne $$self{HASH_DELIM};

  pdebug("entering", 8);
  pIn();

  # Flatten lines using an explicit backslash
  for ($i = 0; $i <= $#lines ; $i++) {

    # Let's disable uninitialized warnings since there's a few 
    # places here we really don't care
    no warnings 'uninitialized';

    if ($lines[$i] =~ /\\\s*$/) {
      pdebug("joining lines @{[ $i + 1 ]} " .  "\& @{[ $i + 2 ]}", 8);
      
      # Lop off the trailing whitespace and backslash, preserving
      # only one space on the assumption that if it's there it's a
      # natural word break.
      $lines[$i] =~ s/(\s)?\s*\\\s*$/$1/;

      # Concatenate the following line (if there is one) after stripping
      # off preceding whitespace
      if ($i < $#lines) {
        $lines[$i + 1]  =~ s/^\s+//;
        $lines[$i]      .= $lines[$i + 1];
        splice(@lines, $i + 1, 1);
        --$i;
      }
    }
  }

  local *saveEntry = sub {
      # Saves the extracted data into the conf hash and resets
      # the vars.

      my ($type);

      ($field, $value) = 
        ($entry =~ /^\s*([^$tagDelim]+?)\s*\Q$tagDelim\E\s*(.*)$/);
      pdebug("saving data:\n\t($field: $value)", 8);

      # Get the field data type from COERCE if set
      if (exists $$self{COERCE}{$field}) {
        $type = $$self{COERCE}{$field};

      # Otherwise, autodetect
      } else {
        $type = scalar quotewords(qr/\s*\Q$hashDelim\E\s*/, 0, $value) > 1 ?
          'hash' : scalar quotewords(qr/\s*\Q$listDelim\E\s*/, 0, $value) > 1 ?
          'list' : 'scalar';
      }
      pdebug("detected type of $field is $type",
        8);

      # We'll apply quotewords to scalar values only if the smart parser is
      # not being used or if we're not coercing all values into scalar for 
      # this field.
      #
      # I hate having to do this but I was an idiot in the previous versions 
      # and this is necessary for backwards compatibility.
      if ($type eq 'scalar') {
        $value = join('', quotewords(qr/\s*\Q$listDelim\E\s*/, 0, $value)) 
          unless $smart && exists $$self{COERCE}{$field} && 
          $$self{COERCE}{$field} eq 'scalar';
      } elsif ($type eq 'hash') {
        $value = { quotewords(qr/\s*(?:\Q$hashDelim\E|\Q$listDelim\E)\s*/, 0,
          $value) };
      } elsif ($type eq 'list') {
        $value = [ quotewords(qr/\s*\Q$listDelim\E\s*/, 0, $value) ];
      }

      # Create the parameter record
      $$conf{$field}              = {};
      $$conf{$field}{Value}       = $value;
      $$conf{$field}{Description} = $comment;
      push(@$order, $field) unless grep /^\Q$field\E$/, @$order;
      $comment = $entry = '';
    };

  # Process lines
  $comment = $entry = '';
  while (defined ($line = shift @lines)) {

    # Grab comments and blank lines
    if ($line =~ /^\s*(?:#.*)?$/) {
      pdebug("comment/blank line:\n\t$line", 9);

      # First save previous entries if $entry has content
      &saveEntry() and $i = 0 if length($entry);

      # Save the comments
      $comment = length($comment) > 0 ? "$comment$line\n" : "$line\n";

    # Grab configuration lines
    } else {

      # If this is the first line of a new entry and there's no
      # PARAM_DELIM skip the line -- something must be wrong.
      #
      # TODO:  Error out/raise exception
      pdebug("skipping spurious text:\n\t$line", 
        9) and next unless length($entry) || $line =~ /\Q$tagDelim\E/;

      # Grab indentation characters and line content
      ($indentation, $data) = ($line =~ /^(\s*)(.+)$/);
      pdebug("data line:\n\t$data", 9);

      # If smart parsing is enabled
      if ($smart) {

        # If there's current content
        if (length($entry)) {

          # If new indentation is greater than original indentation
          # we concatenate the lines as a continuation
          if (length($indentation) > $i) {
            $entry .= $data;

          # Otherwise we treat this a a new entry, so we save the old
          # and store the current
          } else {
            &saveEntry();
            ($i, $entry) = (length($indentation) , $data);
          }

        # No current content, so just store the current data and continue
        # processing
        } else {
          ($i, $entry) = (length($indentation) , $data);
        }

      # Smart parsing is disabled, so treat every line as a new entry
      } else {
        $entry = $data;
        &saveEntry();
      }
    }
  }
  &saveEntry() if length($entry);

  pOut();
  pdebug("leaving w/rv: $rv", 8);

  return $rv;
}

sub _wrap($$$$$) {
  # Parses the passed line of text and inserts indentation and line breaks as
  # specified.
  #
  # Usage:  $paragraph = _wrap(...);

  my $firstIndent = shift;
  my $subIndent   = shift;
  my $textWidth   = shift;
  my $lineBreak   = shift;
  my $paragraph   = shift;
  my (@lines, $segment, $output);

  pdebug("entering w/($firstIndent)" .
    "($subIndent)($textWidth)($lineBreak):\n\t$paragraph", 8);
  pIn();

  # Expand tabs in everything -- sorry everyone
  ($firstIndent)  = expand($firstIndent);
  ($subIndent)    = expand($subIndent);
  $paragraph      = expand("$firstIndent$paragraph");

  $lines[0]       = '';
  while (length($paragraph) > 0) {
    ($segment) = ($paragraph =~ /^(\s*\S+\s?)/);

    # If the segment will fit appended to the current line, concatenate it
    if (length($segment) <= $textWidth - length($lines[$#lines])) {
      $lines[$#lines] .= $segment;

    # Or, if the segment will fit into the next line, add it
    } elsif (length($segment) <= $textWidth - length($subIndent)) {
      $lines[$#lines] .= $lineBreak;
      push(@lines, "$subIndent$segment");

    # Else, split on the text width
    } else {
      $segment = $#lines == 0 ? substr($paragraph, 0, $textWidth) :
        substr($paragraph, 0, $textWidth - length($subIndent));
      if (length($segment) > $textWidth - length($lines[$#lines])) {
        $lines[$#lines] .= $lineBreak;
        push(@lines, ($#lines == 0 ? $segment : "$subIndent$segment"));
      } else {
        $lines[$#lines] .= $segment;
      }
    }
    $paragraph =~ s/^.{@{[length($segment)]}}//;
  }
  $lines[$#lines] .= "\n";

  $output = join('', @lines);

  pOut();
  pdebug("leaving w/rv:\n$output", 8);

  return $output;
}

=head2 hasParameter

  $rv = $conf->hasParameter('FOO');

This function allows you to see if a parameter has been defined or has a
default set for it.  Returns a boolean value.

=cut

sub hasParameter($$) {
  my $self    = shift;
  my $param   = shift;
  my $rv      = 0;
  my @params  = (
                 keys %{ $self->{CONF} },
                 keys %{ $self->{DEFAULTS} },
                );

  croak "Parse::PlainConfig::parameter was called with an undefined parameter"
    unless defined $param;

  pdebug("entering w/($param)", 7);
  pIn();

  $rv = scalar grep /^\Q$param\E$/, @params;

  pOut();
  pdebug("leaving w/rv: $rv", 7);

  return $rv;
}

##################################
# Backwards compatibilty graveyard
##################################

=head1 DEPRECATED METHODS

=head2 delim

  $conf->delim('=');

This method gets and/or sets the parameter name/value delimiter to be used in the 
conf files.  The default delimiter is ':'.  This can be multiple characters.

=cut

sub delim {
  # TODO 2008/05/11: deprecated, remove

  my $self    = shift;
  my $delim   = shift || $self->property('PARAM_DELIM');

  pdebug("Called Parse::PlainConfig::delim -- calling property", 7);
  $self->property(PARAM_DELIM => $delim);
  
  return $delim;
}

=head2 directives

  @directives = $conf->directives;

This method returns a list of all the names of the directives currently 
stored in the configuration hash in no particular order.

=cut

sub directives {
  # TODO 2008/05/11: deprecated, remove

  my $self = shift;

  pdebug("Called Parse::PlainConfig::directives -- calling parameters", 7);

  return $self->parameters;
}

=head2 get

  $field = $conf->get('KEY1');
  ($field1, $field2) = $conf->get(qw(KEY1 KEY2));

The get method takes any number of directives to retrieve, and returns them.  
Please note that both hash and list values are passed by reference.  In order 
to protect the internal state information, the contents of either reference is
merely a copy of what is in the configuration object's hash.  This will B<not>
pass you a reference to data stored internally in the object.  Because of
this, it's perfectly safe for you to shift off values from a list as you
process it, and so on.

=cut

sub get {
  # TODO 2008/05/11: deprecated, remove

  my $self    = shift;
  my $conf    = $$self{CONF};
  my @fields  = @_;
  my (@results, $ref);

  croak "Parse::PlainConfig::get called with no fields" unless @fields;

  pdebug("Entering Parse::PlainConfig::get", 7);
  pIn();

  # Loop through each requested field
  foreach (@fields) {
    $ref = exists $$conf{$_}{Value} ? $$conf{$_}{Value} : undef;
    $ref = { %$ref } if ref($ref) eq 'HASH';
    $ref = [ @$ref ] if ref($ref) eq 'ARRAY';
    push(@results, $ref);
  }

  pOut();
  pdebug("Leaving Parse::PlainConfig::get w/rv: @results", 7);

  # Return the values
  return (scalar @fields > 1) ? @results : $results[0];
}

=head2 set

  $conf->set(KEY1 => 'foo', KEY2 => 'bar');

The set method takes any number of directive/value pairs and copies them into 
the internal configuration hash.

=cut

sub set {
  # TODO 2008/05/11: deprecated, remove

  my $self  = shift;
  my $conf  = $$self{CONF};
  my %new   = (@_);

  foreach (keys %new) { $self->parameter($_, $new{$_}) };

  return 1;
}

=head2 get_ref

  $href = $conf->get_ref

B<Note>:  This used to give you a reference to the internal configuration hash
so you could manipulate it directly.  It now only gives you a B<copy> of the
internal hash (actually, it's reconstructed has to make it look like the old
data structure).  In short, any changes you make to this hash B<will be lost>.

=cut

sub get_ref {
  # TODO 2008/05/11: deprecated, remove

  my $self = shift;
  my $href = {};

  foreach (keys %{ $$self{CONF} }) { $$href{$_} = $$self{CONF}{$_}{Value} };
  pdebug("Called Parse::PlainConfig::get_ref -- rv: $href", 7);

  return $href;
}

=head2 error

  warn $conf->error;

This method returns a zero-length string if no errors were registered with the
last operation, or a text message describing the error.

=cut

sub error {
  # TODO 2008/05/11: deprecated, remove

  my $errStr = ERROR;

  pdebug("Called Parse::PlainConfig::error -- rv: $errStr", 7);

  return $errStr;
}

1;

=head1 DIAGNOSTICS

Through the use of B<Paranoid::Debug> this module will produce internal
diagnostic output to STDERR.  It begins logging at log level 7.  To enable
debugging output please see the pod for Paranoid::Debug.

=head1 HISTORY

  2002/01/18:  Original public release (v1.1)
  2006/05/26:  Complete rewrite (v2.0)

=head1 AUTHOR/COPYRIGHT

(c) 2002 Arthur Corliss (corliss@digitalmages.com) 

=cut

