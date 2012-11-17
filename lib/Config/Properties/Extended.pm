package Config::Properties::Extended;

#######################
# LOAD MODULES
#######################
use strict;
use warnings FATAL => 'all';
use Carp qw(croak carp);

use String::Escape qw(backslash unbackslash);
use String::Util qw(no_space);
use Params::Validate qw(validate_with :types);

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
    list_delimiter => {
        optional => 1,
        type     => SCALAR,
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
        type     => SCALAR,
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

    # Process property references?
    process_references => {
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
        called => 'The ' . __PACKAGE__ . ' Constructor',

        # Options to process
        params => \@args,

        # Normalize key names.
        normalize_keys => $pv_key_normalizer,

        # Do not Allow extra options
        allow_extra => 0,

        # Option Spec
        spec => { %pv_load_spec, %pv_save_spec, },
    );

    # Bless object
    my $self = {
        _options      => \%options,
        _seen_files   => {},
        _current_file => '',
        _properties   => {},
    };
    bless $self, $class;

    # Return object
    return $self;
} ## end sub new

#######################
# PUBLIC METHODS
#######################

#######################
# INTERNAL METHODS
#######################

#######################
# INTERNAL UTILS
#######################

# Seperator regex
sub _sep_regex {

    # Split key-value that is seperated by:
    #   1. '='
    #   2. ':'
    #   3. Whitespace
    # Where neither of them are backslash escaped
    # Also, any surrounding whitespace is ignored
    return qr{\s*(?: (?: (?<!\\) [\=\:\s] ) )\s*}x;
} ## end sub _sep_regex

# Escape key literals
sub _esc_key {
    my ($key) = @_;

    # Escape un-printable
    $key = backslash($key);

    # Escape whitespace
    $key =~ s{(?<!\\)\s}{'\ '}gex;

    # Escape leading '!'
    $key =~ s{^\!}{'\!'}gex;

    return $key;
} ## end sub _esc_key

# Unescape key
sub _unesc_key {
    my ($key) = @_;

    # Escape leading '!'
    $key =~ s{^\\\!}{'!'}gex;

    # Escape whitespace
    $key =~ s{\\\s}{' '}gex;

    # Escape un-printable
    $key = unbackslash($key);

    return $key;
} ## end sub _unesc_key
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
