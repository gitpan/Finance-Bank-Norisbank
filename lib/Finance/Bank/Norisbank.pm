package Finance::Bank::Norisbank;

=head1 NAME

Finance::Bank::Norisbank - Automated account interaction for customers of Norisbank AG.

=cut

$VERSION=0.02;
$DEBUG=0;
use vars '$DEBUG';
use warnings;
use strict;
use Carp;
use HTML::TreeBuilder;
use URI;
use File::Spec;
BEGIN {
    if (0 && require Data::Dump::Streamer) {
	'Data::Dump::Streamer'->import('Dumper');
    } else {
	require Data::Dumper;
	'Data::Dumper'->import('Dumper');
    }
}
use Time::Local;
use WWW::Mechanize;
use HTML::TableExtract;
use POSIX 'ceil', 'floor'; # could reimplement easily enough, but should be pretty universal.
use Storable;
$|=1;

=item new

  Finance::Bank::Norisbank->new('1234567890', '12345');

Connects to the given account number, using the given PIN.  

Norisbank account numbers are ten digits, and PINs are five digits.
Remember that even though account numbers and PINs look like numbers,
they aren't numbers.  Quote them as strings when they appear as
literals in code, or the account number will become a float and lose
precesion on the right, or lose insignificant zeros on the left.

FIXME: More idiomatic interface.

Automatically logs in to check if the account information is valid.

Returns the object on success, or dies on failure.

=cut

sub new {
    my ($class, $accountnr, $pin, $callback) = @_;
    
    my $agent = WWW::Mechanize->new(
	autocheck=>1, # Die on fetch errors
    ) or die "Couldn't create WWW::Mechanize agent";
    
    $agent->env_proxy;
    $callback ||= sub {};
    my $self = bless {accountnr=>$accountnr, pin=>$pin, agent=>$agent, callback=>$callback}, $class;
    
    $self->login;
}

sub dprint {
    print @_ if $DEBUG;
}

sub status {
    my $self=shift;
    $self->{callback}->($self, @_);
}

sub login {
    my $self = shift;
    my $agent=$self->{agent};
    
    $self->status('Getting login form...');
    $agent->get('https://onlinebanking.norisbank.de/norisbank/login.do?method=login');
    $self->status('Logging in...');

    # Defensiveness -- it used to be login, now it's loginForm -- but in both cases, it's the only form on the page.
    $agent->form_number(0);
    $agent->current_form->value('kontonummer', $self->{accountnr});
    $agent->current_form->value('pin', $self->{pin});
    
    $agent->submit();

    if(my $error = parse_login_error ($agent->content))
    {
	die $error;               # parse_login_error($agent->content)
    }

    $self->{last_logged_in}=time;

    $self->status('Logged in.');

    return $self;
}

# Given the content of the page you get after logging in, extracts the
# error, or returns undef/emptylist if there has been no error.
sub parse_login_error {
    my $content = shift;
    $content=$content->content if ref $content;
    
    my $tree=HTML::TreeBuilder->new;
    $tree->no_space_compacting(1); # because norisbank is annoying.
    $tree->parse($content);
    $tree->eof;
    $tree->elementify;
    
    my $error = $tree->look_down(_tag=>'td', class=>'loginfailed');
    if (!$error) {
      $tree->delete;
      return;
    }
    $error = $error->look_down(_tag=>'div')->as_text;
    $error =~ s/^\s*//;
    $error =~ s/\s*$//;
    
    $tree->delete;
    
    if ($error =~ /Die eingegebene Kontonummer ist ung.ltig/) {
      return "Invalid account number";
    } elsif ($error =~ /Die eingegebene webBanking PIN enth.lt unerlaubte Zeichen bzw\. hat eine unzul.ssige L.nge\./) {
      return "PIN wrong length or contains invalid characters";
    } elsif ($error =~ /Die eingegebene PIN ist falsch\./) {
      return "incorrect PIN";
    }
    
    return $error;
}


=item get_transactions

  $account->get_transactions($datefrom, $dateto);

Gets all transactions that occoured (C<bank_date>) between datefrom and dateto,
which should be C<time_ts> (that is, standard unix-ish timestamps).  For your 
convience, 0 is defined as the current date when used as dateto, and 
100 days prior to dateto when used as datefrom.

Note that both ends are rounded to day resolution -- the from date is rounded 
down to midnight at the beginning of the day, and to to midnight at the end of 
the day.  This means that the range used will always completely enclose, but
may exceed, the range asked for.

(This means that C<$account->get_transactions()> gets transactions from 100 
days ago through today.)

FIXME: Make the next paragraph true.

The return in list context is a list of Finance::Bank::Transactions::DE 
objects, least recent first.  If there is an error fetching the information, 
the method will signal an exception (C<die>).  
Return in scalar context is not defined, and subject to change without notice.

=cut

sub get_transactions
{
    my $self     = shift;
    my $datefrom = shift||''; # Avoid undef warnings.
    my $dateto   = shift||'';
    my $agent    = $self->{agent};
    
    dprint "($datefrom, $dateto)\n";

    $dateto = timelocal(0,0,0,$dateto->[1],$dateto->[0]-1,104) 
      if ref($dateto) eq "ARRAY";
    $datefrom = timelocal(0,0,0,$datefrom->[1],$datefrom->[0]-1,104) 
      if ref($datefrom) eq "ARRAY";

    dprint "($datefrom, $dateto)\n";
    
    $dateto = time() if !$dateto;
    dprint "($datefrom, $dateto)\n";
    $datefrom = $dateto-89*24*60*60 if !$datefrom;
    dprint "($datefrom, $dateto)\n";
    
    foreach ($datefrom, $dateto) {
	local $ENV{TZ}="Europe/Rome";
	scalar localtime $_ if $DEBUG;
	my (undef, undef, undef, $day, $mon) = localtime($_);
	$_=sprintf("%02d\.%02d", $day, $mon+1);
	
	dprint " => $_\n";
    }
    
    $self->status('Getting transactions list...', 0, 10);
    $self->status('Getting transactions list...', 1, 10);

    $agent->follow('1') or
        warn $self->parse_error($agent);
    $self->status('Getting transactions list...', 2, 10);
    $agent->follow('Girokonten') or
        warn $self->parse_error($agent);
    $self->status('Getting transactions list...', 3, 10);
    
    $agent->follow('1') or
        warn $self->parse_error($agent);
    $self->status('Getting transactions list...', 4, 10);
    $agent->follow('Umsatzauskunft') or
        warn $self->parse_error($agent);
    $self->status('Getting transactions list...', 5, 10);

    $agent->follow('1') or
        warn $self->parse_error($agent) or
    $self->status('Getting transactions list...', 6, 10);
    $agent->follow('2') or
        warn $self->parse_error($agent);
    $self->status('Getting transactions list...', 7, 10);

#    print $agent->content, "\n\n";
    $agent->form_number(0);
    $agent->current_form->value('von', $datefrom);
    $agent->current_form->value('bis', $dateto);
#    $agent->click('');
    # Norisbank is evil!  We hateses them, we does, yess, yessss.
    $agent->current_form->action('https://onlinebanking.norisbank.de/norisbank/umsatzauskunft.do?method=giroUmsatzauskunft');
    $agent->submit;
    $self->status('Getting transactions list...', 8, 10);
    $agent->follow(1) or
        warn $self->parse_error($agent);
    $self->status('Getting transactions list...', 9, 10);
    $agent->follow(1) or
        warn $self->parse_error($agent);
    $self->status('Getting transactions list...', 10, 10);
    
    my $html = $agent->content;
#    print $html;
    die $self->parse_error($html) if $self->parse_error($html);
    
    $self->status("Parsing transactions list");
    my $tree=HTML::TreeBuilder->new;
    $tree->no_space_compacting(1); # because norisbank is annoying.
    $tree->parse($html);
    $tree->eof;
    $tree->elementify;
    
    my @bghabens = $tree->look_down(_tag=>'a');
    
    $tree->dump if $DEBUG;
    
    if (!@bghabens) {
	dprint $agent->content;
	die "Huh, no transactions?";
    }
    
    # We want only the TR that contains the a tag directly, thus the scalar.
    my @trs = map {scalar $_->look_up('_tag' => 'tr')} @bghabens;
    {
	my %seen;

	@trs = grep {
#	    dprint $_->address, " ", $seen{$_->address}||0, "\n";
	    !$seen{$_->address}++
	} @trs;
    }
    
    dprint "Count \@trs: ".@trs."\n";
    dprint Dumper \@trs;
    
    $self->status("Parsing transactions...", 0, 0+@trs);

    my $n_trans;
    my @transactions = map {
	$self->status("Parsing transactions...", ++$n_trans, 0+@trs);
	my $tr=$_;
	my %trans;
	
	dprint "\n", $tr->address, "\n";
	$tr->dump if $DEBUG;
	
	$trans{address}         = $tr->address;
	$trans{bank_date}       = cleandate($tr->address('.0.0'));
	$trans{effective_date}  = cleandate($tr->address('.1.0'), $trans{bank_date});
	$trans{type}            = ltrim($tr->address('.2.0.0'));
	$trans{thirdparty_name} = ltrim($tr->address('.2.0.2'));
	$trans{amount}          = cleanamount($tr->address('.3.0'));
	$trans{balance}         = cleanamount($tr->address('.4.0'));
	$trans{infourl}         = $tr->address('.2.0')->attr('href');
	
	# 2... doesn't work in perl5, sniff.
	for (2..999) {
	    my $elem = $tr->address('.2.0.'.($_*2));
	    last if !$elem;
	    
	    push @{$trans{comments}}, ltrim($elem);
	}
	
dprint Dumper(\%trans);

	# Translate transaction types
	local $_=$trans{type};
	s/^.berweisung$/liability transfer (local init)/i;      # Customer-initiated transfer
	s/^Lastschrift $/liability transfer (remote init)/i;   # Remote-initiated transfer
	s|^Gehalt/Rente$|payment|i;                  # Remote paying customer
	s/^Gutschrift/asset transfer/i;
	s/^Dauerauftrag/recuring liability transfer/i;          # We told bank to pay them N much every X often.
	s/^Abschlu./banking fee/i;                    # "Closing"
	s/^Euroscheck/euro-check/i;                   # eurocheck
	s/^Scheck/check/i;                            # check
	$trans{type}=$_;
	
	if (defined ($trans{comments}[0])) {
	    grok_cardreader(\%trans);
	    grok_longnumber(\%trans);
	    grok_visa(\%trans);
	    grok_ga(\%trans);
	}

	\%trans;	
    } @trs;

    $self->status("Parsed transactions.");
#    return @transactions;
    return reverse @transactions;
}

=item blz

    $account->blz
    Finance::Bank::Norisbank::->blz
    $account->BLZ
    Finance::Bank::Norisbank::->BLZ

Returns the Bankleihtzahl (sort code) of the bank.

=cut

sub blz {
    return '76026000';
}

*BLZ = *blz;

=item iban

   $account->iban
   $account->IBAN

Returns the IBAN (International Bank Account Number) for this account.

=cut

sub iban {
    return 'DE6276026000'.$_[0]->accountnr;
}

*IBAN = *iban;

=item swift

   $account->swift
   Finance::Bank::Norisbank::->swift

Returns the SWIFT-BIC (Society for Worldwide Interbank Financial Telecommunications - Bank Identification Code) of 
Norisbank.  This is aliased to SWIFT as well.  (This returns the part of the SWIFT code that is the same for all Norisbank 
branches.)

=cut

sub swift {
    return 'NORS DE 71';
}

*SWIFT=*swift;

# Aus Sicherheitgr.nden wurden Sie vom System abgemeldet.

# Given a page with a possible error message on it, return the error
# message (or undef if no error). Can also be given a HTTP::Response or
# WWW::Mechanize agent, for your convience.
sub parse_error {
    my $self = shift;
    my $content = shift;
    if (!defined $content) {
	warn "Undefined content in parse_error (pre ref) called from ".join('/', caller);
    }
    $content=$content->content if ref $content;
    if (!defined $content) {
	warn "Undefined content in parse_error (post ref) called from ".join('/', caller);
    }
    
    my $tree=HTML::TreeBuilder->new;
    $tree->no_space_compacting(1); # because norisbank is annoying.
    $tree->parse($content);
    $tree->eof;
    $tree->elementify;

    # Some pages seem to give a <div class="error"><span class="error">, others just a <div class="error">.
    # We want only the tag that has actual text.
    my $error = $tree->look_down(_tag=>'span', class=>'error');
    $error ||=  $tree->look_down(_tag=>'div',  class=>'error');
    
    $tree->delete, return undef unless $error;
    $error = $error->as_text;
    $tree->delete;
    return undef if $error !~ m/\S/;

    $error =~ s/Die eingegebenen Datumsangaben sind nicht plausibel\./The date range is not possible/;
    $error =~ s/Aus Sicherheitgr.nden wurden Sie vom System abgemeldet\./Login Timeout/;
    $error =~ s/Die Anzahl der Ums.tze, die pro Aufruf angezeigt werden k.nnen, ist auf 100 St.k.*/Too many records returned, truncating at 100./;
    $self->{error} = $error;  
    
    return $error;
}


sub grok_longnumber {
  my $trans=shift;
  
  return unless $trans->{comments}[0] =~ /^\d{27}$/;
  
  # 012345678901234567890123456
  # DDMMHHMMTTTTTTTyymmSSSSLLLL
  
  local $_=$trans->{comments}[0];
  my $day   = substr($_, 0, 2);
  my $month = substr($_, 2, 2);
  my $hour  = substr($_, 4, 2);
  my $min   = substr($_, 6, 2);
  # FIXME: Format does not provide for year (?), so assume this year.
  # FIXME: Assume same year as bank_date?  Assume occourance that happens before bank_date?
  my $year  = (localtime)[5];
  local $ENV{TZ}="Europe/Rome"; # middle-european time.
  
  # Since this format doesn't give the timezone, it's rather unreliable.
  # If the sale_date is already known, don't overwrite it.
  if (!$trans->{sale_date}) {
    $trans->{sale_date}=timelocal(0,$min,$hour,$day,$month-1,$year);
  }
  
  # minimal calls this the transaction number, media markt calls it the POS number
  $trans->{transaction_number}=substr($_, 8, 7);
    
  my ($expireyear, $expiremonth) = (substr($_, 15, 2), substr($_, 17, 2));
  dprint "Expires $expiremonth 20$expireyear?\n";
  
  # Netto gives us zeros for most fields, which kills us for this case.
  if ($expireyear > 0 && $expiremonth > 0) {
      $trans->{card_expiry} = timelocal(0,0,0,1,$expiremonth-1, $expireyear+100);
  }
  
  $trans->{store_number}   =substr($_, 19, 4)+0;
  $trans->{checkout_number}=substr($_, 23, 4)+0;
}

sub grok_cardreader {
  my $trans=shift;
  
  foreach (@{$trans->{comments}}) {
    if ($_ =~ m/^(EC |POZ|OLV|ELV)(\d{8}) (\d\d)\.(\d\d) (\d\d).(\d\d) (...)$/) {
      my %types = (
		   'EC ' => 'Electronic Cash',
		   'POZ' => 'POZ',
		   'OLV' => 'OLV',
	           'ELV' => 'ELV'
		  );
      $trans->{ccard_type} = $types{$1};
      $trans->{cardreader} = $1.$2;
      
      my ($day, $mon, $hour, $min) = ($3,$4,$5,$6);
      my $tz = $7;
      $tz =~ s|ME0|Europe/Rome|;
      local $ENV{TZ}=$tz;
      $trans->{sale_date} = timelocal(0,$min,$hour,$day,$mon-1,(localtime)[5]);
      
      return 1;
    }
  }
}

my $currency_info;
sub grok_visa {
    my $trans=shift;
    
    return unless (my($card_number) = $trans->{thirdparty_name} =~
	m/^VISAUMSATZ (\d{16})/);
    $trans->{ccard_type}='VISA';
    $trans->{ccard_number}=$card_number;
    
    return unless (my($misc1, $day, $mon) = $trans->{comments}[0] =~ 
	m/^(.*) *(\d\d)\.(\d\d)\.$/);
    return unless (my($misc2, $country, $amt) = $trans->{comments}[1] =~
	m/^(.{11}) (..) *?(\d*,\d*)$/);
    
    $trans->{misc}[0]=$misc1;
    local $ENV{TZ}='Europe/Rome';
    $trans->{sale_date} = timelocal(0,0,0,$day,$mon-1,(localtime)[5]);
    
    $trans->{misc}[1]=$misc2;
    $trans->{country}=$country;
    if (!$currency_info) {
	# Load currency info once, keep it in memory thereafter.  It's only
	# about 33k, so this is best-of-both-worlds, and doing it ourselves
	# can keep us from depending on DBI, SQLite, and DateTime (though
	# we may want to use DateTime anyway in the future).	
	
        # The database should be in the same directory as this file. Get
        # the location.
	my (undef, $path) = File::Spec->splitpath(__FILE__);
	my $dbpath = $path . 'currency.storable';
	$currency_info = Storable::retrieve($dbpath);
	if (!$currency_info) {
	    die "Couldn't retrieve $dbpath: $!";
	}
    }
    my $currency = $currency_info->{lc $country};
    $trans->{currency} = $currency;
    $trans->{base_amount} = cleannumber($amt);
    
    for (@{$trans->{comments}}) {
	if (my ($rate) = m/^KURS ZU EURO *?(\d*,\d*)$/) {
	    $trans->{exchange_rate}=cleannumber($rate);
	}
    }
    
    for (@{$trans->{comments}}) {
	if (my ($fees) = 
	    # Note: Does not end in a $.
	    # I suspect the extra space before GEBUEHR that sometimes appears
	    # is a bug either in this module or at the bank.
	    m/^ ?GEBUEHR *(\d*[.,]\d\d)/) {
		$trans->{fees}=cleannumber($fees);
	    }
    }
}

sub grok_ga {
  my $trans=shift;
  
  return unless (my ($num, $blz, $unk) = 
		 $trans->{thirdparty_name} =~
		 /^GA NR(\d{8}) BLZ(\d{8}) (\d)$/);
  $trans->{atm_number}=$num;
  $trans->{atm_blz}=$blz;
  #???
  $trans->{card_number}=$unk;

  $trans->{type}='ATM';

  return unless (my ($day, $month, $hour, $minute, $name) =
		 $trans->{comments}[0] =~
		 m|^(\d\d)\.(\d\d)/(\d\d)\.(\d\d)UHR (.*)|);
  local $ENV{TZ} = "Europe/Rome";
  $trans->{store_date}=timelocal(0,$minute,$hour,$day,$month-1,(localtime)[5]);
  
  $trans->{atm_name}=$name;

  return unless (my ($amt, $fee) =
		 $trans->{comments}[1] =~
		 /^EUR([ \d,.]*)[ *]GEB\.EUR([ \d]\d[,.]\d\d)$/);
  $trans->{base_amount}=cleannumber($amt);
  $trans->{fees}=cleannumber($fee);
}

    
##   <frame name="topFrame" scrolling="NO" noresize src="/jsps/frame_oben.jsp?screen=bankstatus&syd=151317734" >

# Amount allows for thousands seperators, and thus only accepts german style.
sub cleanamount {
    local $_=shift;
    tr/,./.,/;
    tr/-0-9.//cd;
    $_+=0; # Nummify to clean up leading 0s.
    return $_;
}

# Number allows for both german and english style, and thus does not support
# thousands seperators.
sub cleannumber {
    local $_=shift;
    tr/,./../;
    tr/-0-9.//cd;
    $_+=0;
    return $_;
}

sub cleandate {
    local $_=shift;
    croak "cleandate(undef)" if not defined $_;
#    dprint "Date: $_\n";
    tr|.-/|---|;
    tr|-0-9||cd;
    my ($day,$mon,$year)=split(/-/);
    if (not $year) {
	$year = (localtime)[5];
    }
    $_=timelocal(0,0,0,$day,$mon-1,$year);
#    dprint "Date: $_\n";
    return $_;
}

# Remove leading/left whitespace
sub ltrim {
    local $_=shift;
    return undef if not defined $_;
    s/^\s*//;
    $_;
}

1;

=head1 AUTHOR

James Mastros, <james@mastros.biz>.  theorbtwo on perlmonks.org.
  
=head1 BUGS

Known bugs: we aren't always able to get the verwindungszweck from the bank.  
This is because Norisbank's online banking is highly obnixious.  Working code 
to fix this is very much welcome.

The grok_* routines are based on guesswork.  If you think you have them
figured out, or you think there is an error in my interpretation of them,
please let me know.  If you find a standard for them, please, please,
please let me know.

The best place to tell me about bugs or feature requests is probably on
rt.cpan.org.
  
=cut

