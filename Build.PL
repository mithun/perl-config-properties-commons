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
    module_name        => 'Config::Properties::Commons',
    license            => 'perl',
    dist_author        => 'Mithun Ayachit <mithun@cpan.org>',
    configure_requires => { 'Module::Build' => 0.38 },
    build_requires     => {
        'Test::More' => 0,
        'version'    => 0,
    },
    requires => {
        'perl'             => '5.8.1',
        'Encode'           => '2.12',
        'File::Basename'   => 0,
        'File::Slurp'      => 0,
        'File::Spec'       => 0,
        'List::Util'       => 0,
        'Params::Validate' => '1.03',
        'String::Util'     => 0,
        'Text::Wrap'       => 0,
    },
    add_to_cleanup     => ['Config-Properties-Commons-*'],
    create_license     => 1,
    create_makefile_pl => 'traditional',
    meta_merge         => {
        no_index => { directory => ['.author'], },
        keywords  => [qw(java properties apache common config)],
        resources => {
            bugtracker =>
                'http://rt.cpan.org/Public/Dist/Display.html?Name=Config-Properties-Commons',
            repository =>
                'git://github.com/mithun/perl-config-properties-commons.git',
            homepage =>
                'https://github.com/mithun/perl-config-properties-commons',
        },
    },
);

####################
# CREATE BUILD SCRIPT
####################
$builder->create_build_script();
