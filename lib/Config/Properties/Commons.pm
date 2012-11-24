package Config::Properties::Commons;

#######################
# LOAD MODULES
#######################
use strict;
use warnings FATAL => 'all';
use Carp qw(croak carp);

use 5.008_001;
use Encode qw();
use File::Spec qw();
use Text::Wrap qw();
use Cwd qw(abs_path);
use List::Util qw(max);
use File::Slurp qw(read_file);
use File::Basename qw(dirname);
use String::Util qw(no_space fullchomp hascontent trim);
use Params::Validate qw(validate_with validate_pos :types);

#######################
# VERSION
#######################
our $VERSION = '0.01';

#######################
# PARAM SPECS
#######################

# Load spec
my %pv_load_spec = (

    # List delimiter - this identifies multi-token values
    token_delimiter => {
        optional => 1,
        type     => SCALAR | UNDEF,
        default  => ',',
    },

    # Include keyword
    include_keyword => {
        optional => 1,
        type     => SCALAR,
        regex    => qr{^[^\s]+$}x,
        default  => 'include',
    },

    # Include basedir
    includes_basepath => {
        optional => 1,
        type     => SCALAR | UNDEF,
        default  => undef,
    },

    # Process Includes?
    process_includes => {
        optional => 1,
        type     => SCALAR,
        regex    => qr{^[01]$}x,
        default  => 1,
    },

    # Allow recursive includes?
    cache_files => {
        optional => 1,
        type     => SCALAR,
        regex    => qr{^[01]$}x,
        default  => 1,
    },

    # Process property interpolation?
    interpolation => {
        optional => 1,
        type     => SCALAR,
        regex    => qr{^[01]$}x,
        default  => 1,
    },

    # Force values to be array-refs
    force_value_arrayref => {
        optional => 1,
        type     => SCALAR,
        regex    => qr{^[01]$}x,
        default  => 0,
    },

    # Allow callback
    callback => {
        optinal => 1,
        type    => CODEREF,
        default => sub { return @_; },
    },

    # Allow defaults
    defaults => {
        optional => 1,
        type     => HASHREF,
        default  => {},
    },

    # Allow filename for auto-load
    load_file => {
        optional => 1,
        type     => SCALAR | HANDLE | UNDEF,
        default  => undef,
    },
);

# Save Spec
my %pv_save_spec = (

    # Save properties with multiple value tokens on a single line
    save_combine_tokens => {
        optional => 1,
        type     => SCALAR,
        regex    => qr{^[01]$}x,
        default  => 0,
    },

    # Wrap and save
    save_wrapped => {
        optional => 1,
        type     => SCALAR,
        regex    => qr{^[01]$}x,
        default  => 1,
    },

    # Wrap length
    save_wrapped_len => {
        optional => 1,
        type     => SCALAR,
        regex    => qr{^\d+$}x,
        default  => 76,
    },

    # key=value separator
    save_separator => {
        optional => 1,
        type     => SCALAR,
        regex    => qr{^\s*[=:\s]\s*$}x,
        default  => ' = ',
    },

    # Save sorting routine
    save_sorter => {
        optional => 1,
        type     => CODEREF,
        default  => sub ($$) { lc( $_[0] ) cmp lc( $_[1] ); },
    },

    # Save Header
    save_header => {
        optional => 1,
        type     => SCALAR,
        default  => '#' x 15,
    },

    # Save footer
    save_footer => {
        optional => 1,
        type     => SCALAR,
        default  => '#' x 15,
    },
);

# Option aliases
my %option_aliases = (

    # __PACKAGE__
    delimiter      => 'token_delimiter',
    include        => 'include_keyword',
    basepath       => 'includes_basepath',
    includes_allow => 'process_includes',
    cache          => 'cache_files',
    interpolate    => 'interpolation',
    force_arrayref => 'force_value_arrayref',
    validate       => 'callback',
    filename       => 'load_file',
    single_line    => 'save_combine_tokens',
    wrap           => 'save_wrapped',
    columns        => 'save_wrapped_len',
    separator      => 'save_separator',
    header         => 'save_header',
    footer         => 'save_footer',

    # Java Style
    setListDelimiter   => 'token_delimiter',
    setInclude         => 'include_keyword',
    setIncludesAllowed => 'process_includes',
    setBasePath        => 'includes_basepath',
);

# Normalizer
#   Allow leading '-' and make case-insensitive
my $pv_key_normalizer = sub {
    my ($_key) = @_;
    $_key = no_space($_key);
    $_key =~ s{^\-+}{}x;
    $_key = lc($_key);
    return $_key;
};

#######################
# CONSTRUCTOR
#######################
sub new {
    my ( $class, @args ) = @_;

    # Process Options
    my %options = validate_with(

        # Name used in validation errors
        called => __PACKAGE__ . '::new',

        # Options to process
        params => \@args,

        # Normalize key names.
        normalize_keys => $pv_key_normalizer,

        # Do not Allow extra options
        allow_extra => 0,

        # Option Spec
        spec => { %pv_load_spec, %pv_save_spec, },
    );

    # Get default properties
    my %defaults = %{ $options{defaults} };

    # Bless object
    my $self = {
        _options      => \%options,
        _seen_files   => {},
        _current_file => {
            name => '',
            base => '',
        },
        _properties => {%defaults},
    };
    bless $self, $class;

    # Short-circuit _load_ if a filename is defined
    if ( defined $options{load_file} ) {
        $self->load( $options{load_file} );
    }

    # Return object
    return $self;
} ## end sub new

#######################
# PUBLIC METHODS
#######################

# =====================
# LOAD
# =====================
sub load {
    my ( $self, $from, @args ) = @_;
    croak "File name/handle to load from is not provided"
        unless defined $from;

    # Process Options
    my %options = %{ $self->_set_options(@args) };

    unless ( ref $from ) {

        # Not a reference. _should_ be a file

        my $file = $from;

        # Check file
        $file = abs_path($file);
        croak "File $file does not exist!" unless ( $file and -f $file );

        # Set current file
        $self->{_current_file}->{name} = $file;
        $self->{_current_file}->{base} = dirname($file);

        # Process file?
        return 1
            if ($options{cache_files}
            and $self->{_seen_files}->{$file} );

        # Mark as seen
        $self->{_seen_files}->{$file} = 1;
    } ## end unless ( ref $from )

    # Read file
    my @lines = read_file(
        $from,
        binmode => ':utf8',
        chomp   => 1,
    );

    # Load properties
    $self->_load(
        {
            lines   => \@lines,
            options => \%options,
        }
    );

    return 1;
} ## end sub load

# =====================
# GET/SET PROPERTY
# =====================
sub get_property {
    my ( $self, $key ) = @_;
    return unless exists $self->{_properties}->{$key};
    return $self->{_properties}->{$key};
}

sub require_property {
    my ( $self, $key ) = @_;
    my $value = $self->get_property($key);
    croak "Property for $key is not defined" unless defined $value;
    return $value;
} ## end sub require_property

sub add_property {
    my ( $self, @args )   = @_;
    my ( $key,  $values ) = validate_pos(
        @args,
        { type => SCALAR, },
        { type => SCALAR | ARRAYREF, },
    );

    my @new_values;
    my $save      = undef;
    my $old_value = $self->get_property($key);
    @new_values = ref($values) ? @{$values} : ($values);

    if ( defined $old_value ) {
        $save = [ ( ref($old_value) ? @{$old_value} : $old_value ),
            @new_values ];
    }
    else {
        if ( $self->{_options}->{force_value_arrayref} ) {
            $save = [@new_values];
        }
        else {
            if   ( scalar(@new_values) > 1 ) { $save = [@new_values]; }
            else                             { $save = $new_values[0]; }
        }
    } ## end else [ if ( defined $old_value)]

    return unless defined $save;
    $self->{_properties}->{$key} = $save;
    return 1;
} ## end sub add_property

# =====================
# QUERY PROPERTIES
# =====================
sub properties {
    my ( $self, $prefix, $sep ) = @_;

    my %props;
    my %_props = %{ $self->{_properties} };

    if ( defined $prefix ) {
        $sep = '.' unless defined $sep;
        $prefix .= ${sep};
        foreach my $_prop ( grep { /^${prefix}/x } keys %_props ) {
            my $_p = $_prop;
            $_p =~ s{^${prefix}}{}gx;
            $props{$_p} = $_props{$_prop};
        }
    } ## end if ( defined $prefix )
    else {
        %props = %_props;
    }

    return %props if wantarray;
    return {%props};
} ## end sub properties

sub property_names {
    my ( $self, $prefix ) = @_;
    my %props   = $self->properties();
    my $_sorter = $self->{_options}->{save_sorter};
    my @names   = sort $_sorter keys %props;
    if ( defined $prefix ) {
        @names = grep { /^${prefix}/x } @names;
    }
    return @names;
} ## end sub property_names

sub is_empty {
    my ($self) = @_;
    my @keys = $self->property_names();
    return if scalar(@keys);
    return 1;
} ## end sub is_empty

sub has_property {
    my ( $self, @args ) = @_;
    my $val = $self->get_property(@args);
    return 1 if defined $val;
    return;
} ## end sub has_property

# =====================
# CLEAR/DELETE PROPERTY
# =====================
sub delete_property {
    my ( $self, $key ) = @_;
    return
        unless ( defined $key
        and hascontent($key) );

    return 1 unless exists $self->{_properties}->{$key};
    delete $self->{_properties}->{$key};
    return 1;
} ## end sub delete_property

sub clear_properties {
    my ($self) = @_;
    $self->{_properties} = {};
    $self->{_seen_files} = {};
    return 1;
} ## end sub clear_properties

sub reset_property {
    my ( $self, @args ) = @_;
    $self->delete_property(@args) or return;
    $self->add_property(@args)    or return;
    return 1;
} ## end sub reset_property

# =====================
# SAVE PROPERTIES
# =====================
sub save_to_string {
    my ( $self, @args ) = @_;

    # Process Options
    my %options = %{ $self->_set_options(@args) };

    # Get string to save
    my $save_string = $self->_save( { options => \%options, } );

    return $save_string;
} ## end sub save_to_string

sub save {
    my ( $self, $to, @args ) = @_;
    return unless defined $to;

    # Get a string dump
    my $str = $self->save_to_string(@args);

    # Write to file/handle
    write_file( $to, { binmode => ':utf8', }, Encode::encode_utf8($str) );

    # Done
    return 1;
} ## end sub save

# =====================
# FILES PROCESSED
# =====================
sub get_files_loaded {
    my ($self) = @_;
    my @files = sort { lc $a cmp lc $b } keys %{ $self->{_seen_files} };
    return @files;
}

#######################
# METHOD ALIASES
#######################

## no critic (ArgUnpacking)

sub load_fh         { return shift->load(@_); }
sub load_file       { return shift->load(@_); }
sub store           { return shift->save(@_); }
sub save_as_string  { return shift->save_to_string(@_); }
sub saveToString    { return shift->save_to_string(@_); }
sub getProperty     { return shift->get_property(@_); }
sub addProperty     { return shift->add_property(@_); }
sub requireProperty { return shift->require_property(@_); }
sub set_property    { return shift->reset_property(@_); }
sub setProperty     { return shift->reset_property(@_); }
sub changeProperty  { return shift->reset_property(@_); }
sub clear           { return shift->clear_properties(@_); }
sub clearProperty   { return shift->delete_property(@_); }
sub deleteProperty  { return shift->delete_property(@_); }
sub containsKey     { return shift->has_property(@_); }
sub getProperties   { return shift->properties(@_); }
sub subset          { return shift->properties(@_); }
sub getKeys         { return shift->property_names(@_); }
sub propertyNames   { return shift->property_names(@_); }
sub getFileNames    { return shift->get_files_loaded(@_); }
sub isEmpty         { return shift->is_empty(@_); }

## use critic

#######################
# INTERNAL METHODS
#######################

# =====================
# Process options
# =====================
sub _set_options {
    my ( $self, @args ) = @_;

    # Read Options
    my $in_options = {};
    if (@args) {
        if ( ref $args[0] eq 'HASH' ) {
            $in_options = $args[0];
        }
        else {
            $in_options = {@args};
        }
    } ## end if (@args)

    # Merge Options
    my $merged_options = $self->{_options};
    foreach my $_opt ( keys %{$in_options} ) {

        # Normalize
        $_opt = $pv_key_normalizer->($_opt);

        # Resolve Aliases
        if ( exists $option_aliases{$_opt} ) {
            $merged_options->{ $option_aliases{$_opt} }
                = $in_options->{$_opt};
        }
        else {
            $merged_options->{$_opt} = $in_options->{$_opt};
        }
    } ## end foreach my $_opt ( keys %{$in_options...})

    my %valid_options = validate_with(

        # Name used in validation errors
        called => __PACKAGE__ . '::_set_options',

        # Options to process
        params => [$merged_options],

        # Normalize key names.
        normalize_keys => $pv_key_normalizer,

        # Do not Allow extra options
        allow_extra => 0,

        # Option Spec
        spec => { %pv_load_spec, %pv_save_spec, },

    );

    return \%valid_options;
} ## end sub _set_options

# =====================
# Load Properties
# =====================
sub _load {
    my ( $self, $in ) = @_;
    my @lines   = $in->{lines}   ? @{ $in->{lines} }   : ();
    my %options = $in->{options} ? %{ $in->{options} } : ();

    # Check for empty file
    return 1 unless @lines;

    # Check and remote byte order mark
    if ( $lines[0] =~ m{^\x{FEFF}}x ) { shift @lines; }

    # Process lines
    while (@lines) {

        # Get line
        my $line = shift @lines;

        # Remove EOL
        $line = fullchomp($line);

        # Skip Blank
        next unless hascontent($line);

        # Skip Comments
        next if ( $line =~ m{^\s*(?:\#|\!)}x );

        # Trim leading whitespace
        $line = trim( $line, right => 0, );

        # Check for wrapped lines
        if ( $line =~ m{(?<!\\)\\\s*$}x ) {

            # This is a wrapped line. Unwrap
            push( my @wrapped_lines, $line );
            while (@lines) {
                my $_wline = shift @lines;
                $_wline = fullchomp($_wline);
                next unless hascontent($_wline);

                push @wrapped_lines, $_wline;
                last unless ( $_wline =~ m{(?<!\\)\\\s*$}x );
            } ## end while (@lines)

            # Join them
            my @unwrapped;
            foreach my $_wline (@wrapped_lines) {

                # Remove Trailing '\'
                $_wline =~ s{\\\s*$}{}x;

                # Remove leading whitespace
                $_wline = trim( $_wline, right => 0, );

                # Save
                push @unwrapped, $_wline;
            } ## end foreach my $_wline (@wrapped_lines)

            $line = join( '', @unwrapped );
        } ## end if ( $line =~ m{(?<!\\)\\\s*$}x)

        # Split key/value
        my ( $key, $value ) = split( _sep_regex(), $line, 2 );

        # Verify key/value
        if ( ( not( defined $key and defined $value ) )
            or not hascontent($key) )
        {
            croak "Invalid key/value format! : $line \n";
        }

        # Unescape
        $key   = _unesc_key($key);
        $value = _unesc_val($value);

        # Perform callback
        ( $key, $value ) = $options{callback}->( $key, $value );
        next
            unless ( ( defined $key and defined $value )
            and hascontent($key) );

        # Process tokens
        my @tokens;
        if ( defined $options{token_delimiter} ) {
            my $_delim = $options{token_delimiter};
            @tokens = split( qr/(?<!\\) $_delim \s*/x, $value );
        }
        else {
            push( @tokens, $value );
        }

        # Interpolate tokens
        my @interpolated_tokens;
        if ( $options{interpolation} ) {
            foreach my $token (@tokens) {
                $token =~ s{(?<!\\)\$\{([^\}]+)\}}
                    { $self->_interpolate({key => $1, options => \%options,}) }gex;
                push( @interpolated_tokens, $token );
            }
        } ## end if ( $options{interpolation...})
        else {
            push( @interpolated_tokens, @tokens );
        }

        # Process includes
        if ( $options{process_includes}
            and ( $key eq $options{include_keyword} ) )
        {

            my $_basedir = $self->{_current_file}->{base};
            $_basedir = File::Spec->curdir() if not $_basedir;
            $_basedir = $options{includes_basepath}
                if defined $options{includes_basepath};

            foreach my $_file (@interpolated_tokens) {

                # Determine if filename is absolute or relative
                if ( File::Spec->file_name_is_absolute($_file) ) {
                    $_file = abs_path($_file);
                }
                else {
                    $_file = abs_path(
                        File::Spec->catfile( $_basedir, $_file ) );
                }

                # Verify file
                next if not $_file;
                next if not -f $_file;

                # Check if this is the current file being processed
                if ( $_file eq $self->{_current_file}->{name} ) {

                    # Skip it. Otherwise this is an infinite loop
                    next;
                }

                # Load file
                my %tmp_cf = %{ $self->{_current_file} };
                $self->load_file( $_file, \%options );
                $self->{_current_file} = {%tmp_cf};
            } ## end foreach my $_file (@interpolated_tokens)

            # Move onto next line
            # i.e., do not save an 'include'
            next;
        } ## end if ( $options{process_includes...})

        # Save key/value
        my $tmp_fvaf = $self->{_options}->{force_value_arrayref};
        $self->{_options}->{force_value_arrayref}
            = $options{force_value_arrayref};
        $self->add_property( $key, [@interpolated_tokens] );
        $self->{_options}->{force_value_arrayref} = $tmp_fvaf;
    } ## end while (@lines)

    return 1;
} ## end sub _load

# =====================
# Interpolate tokens
# =====================
sub _interpolate {
    my ( $self, $in ) = @_;
    my $key     = $in->{key};
    my %options = %{ $in->{options} };

    # Defaults to original
    my $int_key = '${' . $key . '}';

    # Return if key is not defined
    if ( not defined $self->{_properties}->{$key} ) {
        return $int_key;
    }

    # Get defined key
    my $def_key = $self->{_properties}->{$key};

    # Check if defined key is a refernce
    if ( ref $def_key ) {

        # Return if defined key has multiple values
        return $int_key if ( scalar( @{$def_key} ) > 1 );

        # Do interpolation if we are forcing array refs
        if ( $options{force_value_arrayref} ) {
            $int_key = $def_key->[0];
        }
    } ## end if ( ref $def_key )
    else {
        $int_key = $def_key;
    }

    return $int_key;
} ## end sub _interpolate

# =====================
# Save Properties
# =====================
sub _save {
    my ( $self, $in ) = @_;
    my %options = %{ $in->{options} };

    # Output String
    my $out_str;

    # Get flattened hash
    my %props = $self->properties();

    # Write Header
    $out_str = fullchomp( $options{save_header} ) . "\n\n";

    # Get max property length
    my $max_prop_len = max map { length $_ } ( keys %props );

    # Get key/value separator
    my $out_sep = $options{save_separator};

    # Get separator length
    my $sep_len = length( $options{save_separator} );

    # Do wrap?
    my $do_wrap = $options{save_wrapped};
    $do_wrap = 0
        if ( ( $max_prop_len + $sep_len + 4 ) >= $options{save_wrapped_len} );

    # Cycle thru' properties
    my $_sorter = $options{save_sorter};
    foreach my $key ( sort $_sorter keys %props ) {
        next unless defined $props{$key};
        my $value = $props{$key};

        # Split value into tokens
        my @raw_value_tokens;
        if ( ref($value) ) {
            croak "${key}'s value is an invalid reference!"
                unless ( ref($value) eq 'ARRAY' );
            @raw_value_tokens = @{$value};
        }
        else {
            @raw_value_tokens = ($value);
        }

        # Escape
        $key = _esc_key($key);
        my @value_tokens;
        foreach ( grep { defined $_ } @raw_value_tokens ) {
            push @value_tokens, _esc_val( Encode::encode_utf8($_) );
        }

        # Save
        if ( $options{save_combine_tokens} ) {
            croak "Cannot combine tokens without a delimiter!"
                unless defined $options{token_delimiter};

            # Get delimiter
            # Append a whitespace to it for read-ability
            my $_delim = $options{token_delimiter};
            $_delim .= ' ' unless ( $_delim =~ m{\s+$}x );

            # Join
            my $_val_str = join( $_delim, @value_tokens );

            # Wrap
            if ($do_wrap) {
                $_val_str = _wrap(
                    {
                        string => $_val_str,
                        options =>
                            { %options, key_len => length($key) + $sep_len, },
                    }
                );
            } ## end if ($do_wrap)

            # Write
            $out_str .= sprintf( "%s${out_sep}%s\n", $key, $_val_str );
        } ## end if ( $options{save_combine_tokens...})
        else {

            # Add surrounding blank lines for read-ability
            if ( scalar(@value_tokens) > 1 ) {
                $out_str .= "\n" unless ( $out_str =~ m{\n{2,}}mx );
            }

            foreach my $token (@value_tokens) {
                my $_val_str;

                # Wrap
                if ($do_wrap) {
                    $_val_str = _wrap(
                        {
                            string  => $token,
                            options => {
                                %options, key_len => length($key) + $sep_len,
                            },
                        }
                    );
                } ## end if ($do_wrap)
                else { $_val_str = $token; }

                # Write
                $out_str .= sprintf( "%s${out_sep}%s\n", $key, $_val_str );
            } ## end foreach my $token (@value_tokens)

            # Add surrounding blank lines for read-ability
            if ( scalar(@value_tokens) > 1 ) { $out_str .= "\n"; }
        } ## end else [ if ( $options{save_combine_tokens...})]
    } ## end foreach my $key ( sort $_sorter...)

    # Write footer
    $out_str .= "\n" . fullchomp( $options{save_footer} ) . "\n\n";

    # Done
    return $out_str;
} ## end sub _save

#######################
# INTERNAL UTILS
#######################

# =====================
# Seperator regex
# =====================
sub _sep_regex {

    # Split key-value that is seperated by:
    #   1. '='
    #   2. ':'
    #   3. Whitespace
    # Where neither of them are backslash escaped
    # Also, any surrounding whitespace is ignored
    return qr{\s*(?: (?: (?<!\\) [\=\:\s] ) )\s*}x;
} ## end sub _sep_regex

# =====================
# Escape Routines
# =====================
sub _esc_key {
    my ($key) = @_;

    # Escape unprintable
    $key =~ s{([^\x20-\x7e])}{sprintf ("\\u%04x", ord $1)}gex;

    # Escape leading '#'
    $key =~ s{^\#}{'\#'}gex;

    # Escape leading '!'
    $key =~ s{^\!}{'\!'}gex;

    # Escape whitespace
    $key =~ s{\s}{'\ '}gex;

    return $key;
} ## end sub _esc_key

sub _esc_val {
    my ($val) = @_;

    # Escape unprintable
    $val =~ s{([^\x20-\x7e])}{sprintf ("\\u%04x", ord $1)}gex;

    return $val;
} ## end sub _esc_val

# =====================
# Unescape Routines
# =====================
sub _unesc_key {
    my ($key) = @_;

    # Un-escape unprintable
    $key =~ s{\\u([\da-fA-F]{4})}{chr(hex($1))}gex;

    # Un-escape leading '#'
    $key =~ s{^\\\#}{'#'}gex;

    # Un-escape leading '!'
    $key =~ s{^\\!}{'!'}gex;

    # Un-escape whitespace
    $key =~ s{(?<!\\)\\\s}{' '}gex;

    return $key;
} ## end sub _unesc_key

sub _unesc_val {
    my ($val) = @_;

    # Un-escape unprintable
    $val =~ s{\\u([\da-fA-F]{4})}{chr(hex($1))}gex;

    return $val;
} ## end sub _unesc_val

# =====================
# VALUE WRAPPER
# =====================
sub _wrap {
    my ($in)    = @_;
    my $text    = $in->{string};
    my %options = %{ $in->{options} };

    # Wrap column width
    my $wrap_to = $options{save_wrapped_len} - $options{key_len};

## no critic (PackageVars)

    # Text::Wrap settings
    local $Text::Wrap::columns   = $wrap_to;      # Columns
    local $Text::Wrap::break     = qr{(?<!\\)}x;  # Break at NOT '\'
    local $Text::Wrap::unexpand  = 0;             # Don't mess with tabs
    local $Text::Wrap::separator = "\\\n";        # Use a '\' separator
    local $Text::Wrap::huge = 'overflow';  # Leave unbreakable lines alone

## use critic

    # Wrap
    my $wrapped = Text::Wrap::wrap(
        '',                                # Initial tab is empty
        ' ' x ( $options{key_len} + 1 ), # Subseq tab is aligned to end of key
        $text,                           # Text to wrap
    );

    # Remove EOL
    $wrapped = fullchomp($wrapped);

    # Return
    return $wrapped;
} ## end sub _wrap

#######################
1;

__END__

#######################
# POD SECTION
#######################
=pod

=head1 NAME

Config::Properties::Commons

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 DEPENDENCIES

=head1 BUGS AND LIMITATIONS

Please report any bugs or feature requests to
C<bug-config-properties-commons@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/Public/Dist/Display.html?Name=Config-Properties-Commons>

=head1 AUTHOR

Mithun Ayachit C<mithun@cpan.org>

=head1 LICENSE AND COPYRIGHT

Copyright (c) 2012, Mithun Ayachit. All rights reserved.

This module is free software; you can redistribute it and/or modify it under
the same terms as Perl itself. See L<perlartistic>.

=cut
