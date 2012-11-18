package Config::Properties::Extended;

#######################
# LOAD MODULES
#######################
use strict;
use warnings FATAL => 'all';
use Carp qw(croak carp);

use 5.8.1;
use File::Slurp qw(read_file);
use String::Util qw(no_space fullchomp hascontent trim);
use Params::Validate qw(validate_with :types);
use Cwd qw(abs_path);
use File::Basename qw(dirname);
use File::Spec qw();

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

    # Return object
    return $self;
} ## end sub new

#######################
# PUBLIC METHODS
#######################

# =====================
# Load File
# =====================
sub load_file {
    my ( $self, $file, @args ) = @_;
    croak "Filename not provided!\n" unless $file;

    # Process Options
    my %options = %{ $self->_set_options(@args) };

    # Check file
    $file = abs_path($file);
    croak "File $file does not exist!" unless ( $file and -f $file );

    # Set current file
    $self->{_current_file}->{name} = $file;
    $self->{_current_file}->{base} = dirname($file);

    # Mark as seen
    $self->{_seen_files}->{$file} = 1;

    # Process file?
    return 1
        if ($options{cache_files}
        and $self->{_seen_files}->{$file} );

    # Read file
    my @lines = read_file(
        $file,
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
} ## end sub load_file

# =====================
# Load filehandle
# =====================
sub load_fh {
    my ( $self, $fh, @args ) = @_;
    croak "Input might not be a file handle!\n" unless ref $fh;

    # Process Options
    my %options = %{ $self->_set_options(@args) };

    # Read file
    my @lines = read_file(
        $fh,
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
} ## end sub load_fh

#######################
# INTERNAL METHODS
#######################

# =====================
# Process options
# =====================
sub _set_options {
    my ( $self, @args ) = @_;

    if (@args) {
        if ( ref $args[0] eq 'HASH' ) {
            @args = ( { %{ $self->{_options} }, %{ $args[0] }, } );
        }
        else { @args = ( { %{ $self->{_options} }, @args } ); }
    } ## end if (@args)
    else {
        @args = ( $self->{_options}, );
    }
    my %options = validate_with(

        # Name used in validation errors
        called => __PACKAGE__ . '::_set_options',

        # Options to process
        params => \@args,

        # Normalize key names.
        normalize_keys => $pv_key_normalizer,

        # Do not Allow extra options
        allow_extra => 0,

        # Option Spec
        spec => { %pv_load_spec, %pv_save_spec, },

    );

    return \%options;
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
            $_basedir = $options{includes_basepath}
                if defined $options{includes_basepath};

            foreach my $_file (@interpolated_tokens) {

                # Determine if filename is absolute or relative
                if ( File::Spec->file_name_is_absolute($_file) ) {
                    $_file = abs_path($_file);
                }
                else {
                    $_file = abs_path(
                        File::Spec->catpath( $_basedir, $_file ) );
                }

                # Verify file
                if ( not( $_file or -f $_file ) ) {

                    # Skip it
                    next;
                }

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
        if ( exists $self->{_properties}->{$key} ) {
            my $_old_key = $self->{_properties}->{$key};

            if ( ref $_old_key ) {
                push(
                    @{ $self->{_properties}->{$key} },
                    @interpolated_tokens
                );
            } ## end if ( ref $_old_key )
            else {
                $self->{_properties}->{$key}
                    = [ $self->{_properties}->{$key}, @interpolated_tokens ];
            }
        } ## end if ( exists $self->{_properties...})
        else {
            if ( $options{force_value_arrayref} ) {
                $self->{_properties}->{$key} = [@interpolated_tokens];
            }
            else {
                if ( scalar(@interpolated_tokens) > 1 ) {
                    $self->{_properties}->{$key} = [@interpolated_tokens];
                }
                else {
                    $self->{_properties}->{$key} = $interpolated_tokens[0];
                }
            } ## end else [ if ( $options{force_value_arrayref...})]
        } ## end else [ if ( exists $self->{_properties...})]
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

#######################
1;

__END__

#######################
# POD SECTION
#######################
=pod

=head1 NAME

Config::Properties::Extended

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 DEPENDENCIES

=head1 BUGS AND LIMITATIONS

Please report any bugs or feature requests to
C<bug-config-properties-extended@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/Public/Dist/Display.html?Name=Config-Properties-Extended>

=head1 AUTHOR

Mithun Ayachit C<mithun@cpan.org>

=head1 LICENSE AND COPYRIGHT

Copyright (c) 2012, Mithun Ayachit. All rights reserved.

This module is free software; you can redistribute it and/or modify it under
the same terms as Perl itself. See L<perlartistic>.

=cut
