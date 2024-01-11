#!/usr/bin/env perl

# usage: cat kana101.txt | perl hhkbfig.pl > kana101.svg

# 2024-01-09
# 2024-01-04 for yoko50.txt
# 2023-12-28 renamed from kana101.pl
# 2023-12-27 Delete
# 2023-12-26 from wmacs.pl

# --------------------------------------------------------------------
# use

use utf8;
use Encode qw(decode encode);

use strict;
use warnings;

# --------------------------------------------------------------------
# encode/decode

sub enc($) {encode("utf-8", shift);}

sub dec($) {decode("utf-8", shift);}

# --------------------------------------------------------------------

my $c_sep0 = qq(-);
my $c_sep1 = qq(¦);
my $c_sep = qq($c_sep0$c_sep1);

# --------------------------------------------------------------------

sub strip($) {
    $_[0] =~ s/^\s*(.*?)\s*$/$1/;
    $_[0];
}

# --------------------------------------------------------------------
# Common

{
    package Keyname::Common;

    our %keysym = qw(Lt ← Dn ↓ Up ↑ Rt → Spc Space Ret Enter PSc PrtSc quot quote);

    # our %unmodKeysym = qw(Ret Enter LC Control LS Shift RS Shift LA Alt LM Meta RM Meta RA Alt LW Win RW Win RC Ctrl);
    our %unmodKeysym = qw(Ret Return LC Control LS Shift RS Shift LA Alt LM ◇ RM ◇ RA Alt LW Win RW Win RC Ctrl);
    $unmodKeysym{"Spc"} = "";

    our %shiftedKeyname;
    {
        no warnings "qw";
        %shiftedKeyname = qw(! 1 @ 2 # 3 \$ 4 % 5 ^ 6 & 7 * 8 ( 9 ) 0 _ - + = | \\ ~ ` { [ } ] : ; " ' < , > . ? /);
    }
}

# --------------------------------------------------------------------

sub do_check($\@) {
    my ($key, $keyname) = @_;
    if (!grep {$_ eq $key} @$keyname) {
        print STDERR "unknown key: $key\n";
    }
}

# --------------------------------------------------------------------
# 読み込み

sub do_read() {
    my $buf = "";
    my @buf = ();
    #
    while(<>) {
        chomp;
        s/\r//;
        $_ = &dec($_);
        $buf .= "$_\n";
    }
    #
    @buf = split /\n/, $buf;
    @buf;
}

# --------------------------------------------------------------------
# 定義

my @g_obj;

sub do_def(@) {
    my @buf = @_;
    my @ashift;
    my @aunshift;
    my $shift;
    #
    @g_obj = ();
    #
    for (@buf) {
        # 横罫線の行
        if (m/^[$c_sep0]+$/) {
            my @a = @ashift;
            my @b = @aunshift;
            for (my $i = 0; $i < @b; $i++) {
                my ($a, $b) = ($a[$i], $b[$i]);
                next unless defined $a && defined $b;
                # 空は読み飛ばす
                &strip($a); &strip($b);
                next if $b eq "";
                # unshift 側
                # my ($unmodunshift, $modunshift) = split / /, $b;
                my ($unmodunshift, $modunshift) = split /(?: |(?![ -~]))/, $b;
                # shift 側
                # my ($unmodshift, $modshift) = split / /, $a;
                my ($unmodshift, $modshift) = split /(?: |(?![ -~]))/, $a;
                #
                # XXX
                $modunshift = "" if !defined $modunshift;
                $modshift = "" if !defined $modshift;
                my @o = ($unmodunshift, $unmodshift, $modunshift, $modshift);
                pop @o if !defined $o[-1];
                push @g_obj, \@o;
            }
            #
            $shift = 1;
        }
        # 定義している行
        elsif (m/^[$c_sep1]/) {
            # shift 側
            if ($shift) {@ashift = split /[$c_sep1]/;}
            # unshift 側
            else {@aunshift = split /[$c_sep1]/;}
            $shift = $shift ? 0 : 1;
        }
    }
}

# --------------------------------------------------------------------
# svg

my $g_keySizeUnit = 52;
my $g_keySizeUnit1 = 40;
my $g_keyMargin = 1;
my $g_fontSize = 12;

sub do_svg() {
    my $kbdWidth = $g_keySizeUnit * 15;
    my $kbdHeight = $g_keySizeUnit * 5;
    my $kbdMarginH = 10;
    my $kbdMarginV = 10;
    my $canvasWidth = $kbdWidth + $kbdMarginH * 2;
    my $canvasHeight = $kbdHeight + $kbdMarginV * 2;

    my $svg;
    my $g;

    $svg = SVG->new(width => $canvasWidth, height => $canvasHeight);

    $svg->rectangle(x => 0, y => 0, width => $canvasWidth, height => $canvasHeight, style => {"fill" => "#eeeeee"});

    my $fontSize = "${g_fontSize}px";
    # my $fontFamily = "Verdana";
    my $fontFamily = "Verdana, HiraginoSans-W4";
    $g = $svg->group(transform => "translate($kbdMarginH, $kbdMarginV)", style => {"font-family" => $fontFamily, "font-size" => $fontSize});

    for (@g_obj) {&svg_putKey($g, $_);}

    print &enc($svg->xmlify . "\n");

}

sub svg_key($$$;\@$$$) {
    my ($g, $row, $col, $ary, $wd, $bg1, $bg2) = @_;
    $bg1 = "#fcfcfc" unless defined $bg1;
    $bg2 = "#cccccc" unless defined $bg2;
    my $u = $g_keySizeUnit;
    my $m = $g_keyMargin;
    my ($x, $y, $w, $h);
    $x = $col * $u + 0.5;
    $y = (4 - $row) * $u + 0.5;
    $w = $wd * $u - $m;
    $h = $u - $m;
    $g->rectangle(x => $x, y =>$y, width => $w, height => $h, rx => 5, ry => 5, style => {"fill" => $bg2, "stroke" => "#000000", "stroke-width" => 1});
    my $p = ($g_keySizeUnit - $g_keySizeUnit1) / 2;
    $x += $p;
    $y += $p / 2;
    $w = $wd * $u - $m - $p * 2;
    $h = $g_keySizeUnit1;
    if ($bg1 && $bg1 ne "none") {
        $g->rectangle(x => $x, y => $y, width => $w, height => $h, rx => 3, ry => 3, style => {"fill" => $bg1});
    }
    #
    my @a = @$ary;
    my $q = 2;
    $x += $q;
    $y += $q + $g_fontSize;
    $y += $g_keySizeUnit1 / 2 - $q;
    return if @a < 1;
    my $dy = !$a[1] ? -($g_keySizeUnit1 / 2 - $q) / 2 : 0;
    $g->text(x => $x, y => $y + $dy)->cdata(defined $Keyname::Common::unmodKeysym{$a[0]} ? $Keyname::Common::unmodKeysym{$a[0]} : $a[0]) if !(1 < @a && defined $a[1] && $a[0] eq lc $a[1]) && $a[0] ne "";
    return if @a < 2;
    $y -= $g_keySizeUnit1 / 2 - $q;
    $g->text(x => $x, y => $y)->cdata($a[1]) if defined $a[1] && $a[1] ne "";
    return if @a < 3;
    $x += $g_keySizeUnit1 / 2 - $q;
    $y += $g_keySizeUnit1 / 2 - $q;
    $g->text(x => $x, y => $y)->cdata($a[2]) if $a[2] ne "";
    return if @a < 4;
    $y -= $g_keySizeUnit1 / 2 - $q;
    $g->text(x => $x, y => $y)->cdata($a[3]) if $a[3] ne "";
}

my $g_row;
my $g_col;

sub svg_putKey($\@;$$$$$) {
    my ($g, $ary, $row, $col, $wd, $bg1, $bg2) = @_;
    if ($$ary[0] eq "Esc") {($row, $col, $wd) = (4, 0, 1.0);}
    if ($$ary[0] eq "Tab") {($row, $col, $wd) = (3, 0, 1.5);}
    if ($$ary[0] eq "BS" ) {($row, $col, $wd) = (undef, undef, 1.5);}
    if ($$ary[0] eq "Delete") {($row, $col, $wd) = (undef, undef, 1.5);}
    if ($$ary[0] eq "LC" ) {($row, $col, $wd) = (2, 0, 1.75);}
    if ($$ary[0] eq "Ret") {($row, $col, $wd) = (undef, undef, 2.25);}
    if ($$ary[0] eq "LS" ) {($row, $col, $wd) = (1, 0, 2.25);}
    if ($$ary[0] eq "RS" ) {($row, $col, $wd) = (undef, undef, 1.75);}
    if ($$ary[0] eq "Fn" ) {($row, $col, $wd) = (undef, undef, 1.0);}
    if ($$ary[0] eq "LA" ) {($row, $col, $wd) = (0, 1.5, 1.0);}
    if ($$ary[0] eq "LM" || $$ary[0] eq "LW") {($row, $col, $wd) = (undef, undef, 1.5);}
    if ($$ary[0] eq "Spc") {($row, $col, $wd) = (undef, undef, 6.0);}
    if ($$ary[0] eq "RM" || $$ary[0] eq "RW") {($row, $col, $wd) = (undef, undef, 1.5);}
    if ($$ary[0] eq "RA") {($row, $col, $wd) = (undef, undef, 1.0);}
    if ($$ary[0] eq "RC") {($row, $col, $wd, $bg1, $bg2) = (undef, undef, 1.0, "#c3d8b7", "#9cb090");}
    $wd = 1.0 unless defined $wd;
    $bg1 = "#fcfcfc" unless defined $bg1;
    $bg2 = "#cccccc" unless defined $bg2;
    $g_row = $row if defined $row;
    $g_col = $col if defined $col;
    #
    &svg_key($g, $g_row, $g_col, $ary, $wd, $bg1, $bg2);
    #
    $g_col += $wd;
}

# --------------------------------------------------------------------
# 出力

&do_def(&do_read);

use SVG;
&do_svg();
