#!/usr/bin/perl
use strict;
use warnings;
use Test::More 'no_plan';

require_ok('Finance::Bank::Norisbank');

is(Finance::Bank::Norisbank::cleanamount('1'),          1,    'cleanamount:      1');
is(Finance::Bank::Norisbank::cleanamount('-1'),        -1,    'cleanamount:     -1');
is(Finance::Bank::Norisbank::cleanamount('1,01'),       1.01, 'cleanamount:   1,01');
is(Finance::Bank::Norisbank::cleanamount('   1'),       1,    'cleanamount: "    1"');
is(Finance::Bank::Norisbank::cleanamount('1.000'),   1000,    'cleanamount: "1.000"');
is(Finance::Bank::Norisbank::cleanamount('1.000,01'),1000.01, 'cleanamount: "1.000,01"');

is(Finance::Bank::Norisbank::cleannumber('1000.01'), 1000.01, 'cleannumber: 1000.01');
is(Finance::Bank::Norisbank::cleannumber('1000,01'), 1000.01, 'cleannumber: 1000,01');
    

my $trans = {
   comments => [
   	    # first of febuary, 3:04 AM.
	    # Transaction number 1234567
	    # Expires Dec 1, 2010.
	    # Store number 6666
	    # Checkout number 7777
   	    '010203041234567120166667777'
   ]
};
Finance::Bank::Norisbank::grok_longnumber($trans);
is(scalar gmtime $trans->{sale_date}, 
    'Sun Feb  1 02:04:00 2004', 
    'longnumber: sale_date');
is($trans->{transaction_number}, 1234567, 
    'longnumber: transaction_number');
is(scalar localtime $trans->{card_expiry}, 
    'Sun Jan  1 00:00:00 2012', 
    'longnumber: card_expiry');
is($trans->{store_number}, 6666, 
    'longnumber: store_number');
is($trans->{checkout_number}, 7777, 
    'longnumber: checkout_number');

$trans = {
   comments => [
   	    # first of febuary, 3:04 AM.
	    # Transaction number 1234567
	    # Expires Dec 1, 2010.
	    # Store number 6666
	    # Checkout number 7777
   	    '01020000000000000000007777'
   ]
};
eval { Finance::Bank::Norisbank::grok_longnumber($trans); };
isnt($@, 'grok_longnumber: zeros in the middle');

$trans = {
   comments => ['EC 68069870 05.03 15.38 ME0']
};
Finance::Bank::Norisbank::grok_cardreader($trans);
is($trans->{cardreader}, 'EC 68069870',     'cardreader: cardreader');
is(scalar gmtime($trans->{sale_date}),
    'Fri Mar  5 14:38:00 2004',
    'cardreader: sale_date');
is($trans->{ccard_type}, 'Electronic Cash', 'cardreader: ccardtype (EC)');

$trans = {
   comments => ['', 'POZ68069870 05.03 15.38 ME0']
};
Finance::Bank::Norisbank::grok_cardreader($trans);
is($trans->{ccard_type}, 'POZ', 'cardreader: ccardtype (POZ), second line');

$trans = {
    thirdparty_name =>
    'VISAUMSATZ 1234123412341234',
   comments => [
       'RYANAIR INTE0000000N 01.01.',
       'DUBLIN      IE       123,45',
       'GEBUEHR        0.00']
};
Finance::Bank::Norisbank::grok_visa($trans);
is($trans->{ccard_type},   'VISA', 'visa: ccard_type');
is($trans->{ccard_number}, '1234123412341234', 'visa: ccard_number');
is($trans->{misc}[0],      'RYANAIR INTE0000000N ', 'visa: misc[0]');
is(scalar gmtime $trans->{sale_date}, 'Wed Dec 31 23:00:00 2003', 'visa: sale_date');
is($trans->{misc}[1],      'DUBLIN     ', 'visa: misc[1]');
is($trans->{country},      'IE', 'visa: country');
is($trans->{currency},     'EUR', 'visa: currency');
is($trans->{base_amount},  123.45, 'visa: base_amount');
is($trans->{fees},         0, 'visa: fees');


