#!perl

####################
# LOAD MODULES
####################
use strict;
use warnings FATAL => 'all';
use Module::Build;

# Autoflush ON
local $| = 1;

####################
# INIT BUILDER
####################

my $builder = Module::Build->new(
    module_name        => 'Config::Properties::Extended',
    license            => 'perl',
    dist_author        => 'Mithun Ayachit <mithun@cpan.org>',
    configure_requires => { 'Module::Build' => 0.38 },
    build_requires     => {
        'Test::More' => 0,
        'version'    => 0,
    },
    requires => {

    },
    add_to_cleanup     => ['Config-Properties-Extended-*'],
    create_license     => 1,
    create_makefile_pl => 'traditional',
    meta_merge         => {
        keywords  => [qw()],
        resources => {
            bugtracker =>
                'http://rt.cpan.org/Public/Dist/Display.html?Name=Config-Properties-Extended',
            repository => 'git://github.com/mithun/perl-config-properties-extended.git',
            homepage   => 'https://github.com/mithun/perl-config-properties-extended',
        },
    },
);

####################
# CREATE BUILD SCRIPT
####################
$builder->create_build_script();
