package Proc::SafeExec;

use 5.006;
use strict;
use warnings;

our $VERSION = '1.2';

=pod

=head1 NAME

Proc::SafeExec - Convenient utility for executing external commands in various ways.

=head1 SYNOPSIS

	use Proc::SafeExec;
	my $command = new Proc::SafeExec({
		# Choose just one of these.
		exec => ["ls", "-l", "myfile"],  # exec() after forking.
		fork => 1,                       # Return undef in the child after forking.

		# Specify whether to capture each.  Specify a file handle ref to dup an
		# existing one.  Specify "new" to create a new file handle, "default" or undef
		# to keep the parent's descriptor, or "close" to close it.
		stdin => \*INPUT_PIPE,
		stdout => \*OUTPUT_PIPE,
		stderr => "new",

		# Miscellaneous options.
		child_callback => \&fref,  # Specify a function to call in the child after fork(), for example, to drop privileges.
		debug => 1,  # Emit some information via warnings, such as the command to execute.
		no_autowait => 1,  # Don't automatically call $command->wait() when $command is destroyed.
		real_arg0 => "/bin/ls",  # Specify the actual file to execute.
		untaint_args => 1,  # Untaint the arguments before exec'ing.
	});
	printf "Child's PID is %s\n", $command->child_pid if $command->child_pid;

The wait method waits for the child to exit or checks whether it already
exited:

	$command->wait({
		# Optional hash of options.
		no_close => 1,  # Don't close "new" file handles.
		nonblock => 1,  # Don't wait if the child hasn't exited (implies no_close).
	});

To communicate with the child:

	# Perl doesn't understand <$command->stdout>.
	my $command_stdout = $command->stdout;
	my $command_stderr = $command->stderr;

	$line = <$command_stdout>;
	$line = <$command_stderr>;
	print {$command->stdin} "mumble\n";

To check whether the child exited yet:

	print "Exit status:  ", $command->exit_status, "\n" if $command->wait({nonblock => 1});

To wait until it exits:

	$command->wait();
	print "Exit status:  ", $command->exit_status, "\n";

=head1 DESCRIPTION

Proc::SafeExec provides an easy, safe way to execute external programs.  It
replaces all of Perl's questionable ways of accomodating this, including
system(), open() with a pipe, exec(), back-ticks, etc.  This module will never
automatically invoke /bin/sh.  This module is easy enough to use that /bin/sh
should be unnecessary, even for complex pipelines.

For all errors, this module dies setting $@.

Errors from exec() in the child are reported gracefully to the parent.  This
means that if anything fails in the child, the error is reported through $@
with die just like any other error.  This also reports $@ if child_callback
dies when it is called between fork() and exec().  This is accomplished by
passing $@ through an extra pipe that's closed when exec succeeds.  Note:  A
side-effect of this is $@ is stringified if it isn't a string.

=head1 CAVEATS

When using an existing file handle by passing a reference for stdin, stdout, or
stderr, new() closes the previously open file descriptor.  This is to make
sure, for example, that when setting up a pipeline the child process notices
EOF on its stdin.  If you need this file handle to stay open, dup it first.
For example:

	open my $tmp_fh, "<&", $original_fh or die "dup:  $!";
	my $ls = new Proc::SafeExec({exec => ["ls"], stdout => $tmp_fh});
	# $tmp_fh is now closed.

By default, $command->wait() closes any new pipes opened in the constructor.
This is to prevent a deadlock where the child is waiting to read or write and
the parent is waiting for the child to exit.  Pass no_close to $command->wait()
to prevent this (see above).  Also, by default the destructor calls
$command->wait() if child hasn't finished.  This is to prevent zombie processes
from inadvertently accumulating.  To prevent this, pass no_autowait to the
constructor.  The easiest way to wait for the child is to call the wait method,
but if you need more control, set no_autowait, then call child_pid to get the
PID and do the work yourself.

=head1 EXAMPLES

It's easy to execute several programs to form a pipeline.  For the first
program, specify "new" for stdout.  Then execute the second one, and specify
stdout from the first one for the stdin of the second one.  For example, here's
how to write the equivalent of system("ls | sort > output.txt"):

	open my $output_fh, ">", "output.txt" or die "output.txt:  $!\n";
	my $ls = new Proc::SafeExec({exec => ["ls"], stdout => "new"});
	my $sort = new Proc::SafeExec({exec => ["sort"], stdin => $ls->stdout, stdout => $output_fh});
	$ls->wait();
	$sort->wait();
	printf "ls exited with status %i\n", ($ls->exit_status >> 8);
	printf "sort exited with status %i\n", ($sort->exit_status >> 8);

=head1 INSTALLATION

This module has no dependencies besides Perl itself.  Follow your favorite
standard installation procedure.

To test the module, run the following command line:

	$ perl -e 'use Proc::SafeExec; print Proc::SafeExec::test();'

=head1 VERSION AND HISTORY

=over

=item * Version 1.2, released 2008-01-22.  Tweaked test() to handle temp files
correctly, addressing https://rt.cpan.org/Ticket/Display.html?id=32458 .

=item * Version 1.1, released 2008-01-09.  Fixed obvious bug.

=item * Version 1.0, released 2007-05-23.

=back

=head1 SEE ALSO

The source repository is at git://git.devpit.org/Proc-SafeExec/

=head1 AUTHOR

Leif Pedersen, E<lt>bilbo@hobbiton.orgE<gt>

=head1 COPYRIGHT AND LICENSE

 This may be distributed under the terms below (BSD'ish) or under the GPL.
 
 Copyright (c) 2007
 All Rights Reserved
 Meridian Environmental Technology, Inc.
 4324 University Avenue, Grand Forks, ND 58203
 http://meridian-enviro.com
 
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are
 met:
 
  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
 
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the
     distribution.
 
 THIS SOFTWARE IS PROVIDED BY AUTHORS AND CONTRIBUTORS "AS IS" AND ANY
 EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL AUTHORS OR CONTRIBUTORS BE
 LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=cut

use Fcntl "F_GETFL", "F_SETFL", "FD_CLOEXEC";
use File::Spec;
use File::Temp;
use POSIX "WNOHANG";

sub new {
	my ($package, $options) = @_;

	my $class_name = ref($package) || $package;
	my $self = {};
	bless $self, $class_name;

	# Be sure we don't gain extra references to any file handles or clobber
	# anything the caller needs.
	$options = {%$options};

	# Usage checks; set defaults.
	$self->{"debug"} = $options->{"debug"};
	$options->{"stdin"} = "default" unless defined $options->{"stdin"};
	$options->{"stdout"} = "default" unless defined $options->{"stdout"};
	$options->{"stderr"} = "default" unless defined $options->{"stderr"};
	die "No action specified for child process\n" unless $options->{"exec"} or $options->{"fork"};
	die "More than one action specified for child process\n" if $options->{"exec"} and $options->{"fork"};
	warn "Executing:  @{$options->{'exec'}}\n" if $self->{"debug"} and $options->{"exec"};
	if($options->{"exec"}) {
		my $count = -1;
		while(++$count < @{$options->{"exec"}}) {
			die "Argument ${count} to exec is undef\n" unless defined $options->{"exec"}[$count];
		}
	}

	# Regarding file handles, $self holds the side that the parent will see and
	# $options holds the side that the child will see.  Remember, if we're passed a
	# file handle reference, the parent closes it after passing it to the child.

	# Prepare file descriptors.
	if(ref $options->{"stdin"}) {
		# Empty
	} elsif($options->{"stdin"} eq "new") {
		$self->{"stdin"} = undef;
		$options->{"stdin"} = undef;
		# Careful of the order.  It's pipe README, WRITEME.
		pipe $options->{"stdin"}, $self->{"stdin"} or die "pipe:  $!\n";
		_set_cloexec($self->{"stdin"});
		_set_cloexec($options->{"stdin"});
	} elsif($options->{"stdin"} eq "close") {
		# Empty
	} elsif($options->{"stdin"} eq "default") {
		# Empty
	} else {
		die "Unknown option for stdin:  $options->{'stdin'}\n";
	}
	if(ref $options->{"stdout"}) {
		# Empty
	} elsif($options->{"stdout"} eq "new") {
		$self->{"stdout"} = undef;
		$options->{"stdout"} = undef;
		# Careful of the order.  It's pipe README, WRITEME.
		pipe $self->{"stdout"}, $options->{"stdout"} or die "pipe:  $!\n";
		_set_cloexec($self->{"stdout"});
		_set_cloexec($options->{"stdout"});
	} elsif($options->{"stdout"} eq "close") {
		# Empty
	} elsif($options->{"stdout"} eq "default") {
		# Empty
	} else {
		die "Unknown option for stdout:  $options->{'stdout'}\n";
	}
	if(ref $options->{"stderr"}) {
		# Empty
	} elsif($options->{"stderr"} eq "new") {
		$self->{"stderr"} = undef;
		$options->{"stderr"} = undef;
		# Careful of the order.  It's pipe README, WRITEME.
		pipe $self->{"stderr"}, $options->{"stderr"} or die "pipe:  $!\n";
		_set_cloexec($self->{"stderr"});
		_set_cloexec($options->{"stderr"});
	} elsif($options->{"stderr"} eq "close") {
		# Empty
	} elsif($options->{"stderr"} eq "default") {
		# Empty
	} else {
		die "Unknown option for stderr:  $options->{'stderr'}\n";
	}

	# Set the close-on-exec flag for both ends in both processes since the child
	# indicates the success of exec() by closing the pipe.
	pipe my $error_pipe_r, my $error_pipe_w or die "pipe:  $!\n";
	_set_cloexec($error_pipe_r);
	_set_cloexec($error_pipe_w);
	select((select($error_pipe_w), $| = 1)[0]);  # Set autoflushing for writing.

	$self->{"child_pid"} = fork();
	die "fork:  $!\n" unless defined $self->{"child_pid"};

	if($self->{"child_pid"}) {
		# Parent
		$error_pipe_w = undef;

		close $options->{"stdin"} if ref $options->{"stdin"};
		close $options->{"stdout"} if ref $options->{"stdout"};
		close $options->{"stderr"} if ref $options->{"stderr"};

		# EOF indicates no error.  This blocks until exec() succeeds or fails.  When
		# $error_pipe_r falls out of scope, it's closed.  Also, when $self isn't
		# returned, the last ref is lost and DESTROY is called.
		if(defined (my $err = <$error_pipe_r>)) {
			chomp $err;
			die "$err\n";
		}

		# Don't set this until just before returning because if the constructor
		# dies, the child must be cleaned.
		$self->{"no_autowait"} = $options->{"no_autowait"};

		return $self;
	}

	# Child

	# Trap dies and force the child to exit instead because the caller isn't
	# expecting both to return.
	eval {
		$error_pipe_r = undef;

		# This can matter if the child isn't going to call exec(), since the object is
		# then destroyed when the child returns.
		$self->{"no_autowait"} = 1;

		# Set up the child's file descriptors.
		if(ref $options->{"stdin"}) {
			# Also covers "new".  See above.
			untie *STDIN;  # Some programs, like mod_perl, think it's comical to tie packages to file handles.
			open STDIN, "<&", $options->{"stdin"} or die "dup:  $!\n";
			close $options->{"stdin"};
		} elsif($options->{"stdin"} eq "close") {
			close STDIN;
		} elsif($options->{"stdin"} eq "default") {
			# Empty
		} else {
			die "Can't happen!";
		}
		if(ref $options->{"stdout"}) {
			# Also covers "new".  See above.
			untie *STDOUT;  # Some programs, like mod_perl, think it's comical to tie packages to file handles.
			open STDOUT, ">&", $options->{"stdout"} or die "dup:  $!\n";
			close $options->{"stdout"};
		} elsif($options->{"stdout"} eq "close") {
			close STDOUT;
		} elsif($options->{"stdout"} eq "default") {
			# Empty
		} else {
			die "Can't happen!";
		}
		if(ref $options->{"stderr"}) {
			# Also covers "new".  See above.
			untie *STDERR;  # Some programs, like mod_perl, think it's comical to tie packages to file handles.
			open STDERR, ">&", $options->{"stderr"} or die "dup:  $!\n";
			close $options->{"stderr"};
		} elsif($options->{"stderr"} eq "close") {
			close STDERR;
		} elsif($options->{"stderr"} eq "default") {
			# Empty
		} else {
			die "Can't happen!";
		}

		# Lose unnecessary references to these.  (This closes the other end of pipes.)
		$self->{"stdin"} = undef;
		$self->{"stdout"} = undef;
		$self->{"stderr"} = undef;

		# XXX: I didn't document that $error_pipe_w is passed to child_callback because
		# I haven't decided whether it's a good idea.  This allows the caller to
		# unblock the parent by closing the pipe if it needs to do something that never
		# returns.  However, if it does close the pipe, it must never return.  This
		# allows the caller to take advantage of this module's logic without any
		# intention to ever call exec() after fork().  It can also be useful for
		# suspending execution of the parent until the task is complete while reporting
		# errors to the parent via die(), if it does NOT close the pipe.
		&{$options->{"child_callback"}}({error_pipe => $error_pipe_w}) if $options->{"child_callback"};

		if($options->{"exec"}) {
			$options->{"real_arg0"} = ${$options->{"exec"}}[0] unless defined $options->{"real_arg0"};

			# Untaint just the arg list, not $options->{"real_arg0"}.
			if($options->{"untaint_args"}) {
				foreach my $arg (@{$options->{"exec"}}) {
					($arg) = ($arg =~ qr/^(.*)$/s);
				}
			}

			{
				# exec {$arg0} @args will never add the shell interpreter.  This handles the
				# errors from exec, so tell Perl not to report them.
				no warnings 'exec';
				exec {$options->{"real_arg0"}} @{$options->{"exec"}};
			}
			die "$options->{'real_arg0'}:  $!\n";
		}

		if($options->{"fork"}) {
			return ();
		}

		die "Can't happen!  No action specified for child process, checked in parent";
	};
	if($@) {
		print $error_pipe_w $@;
		POSIX::_exit(1);
	}
	# This return path happens if $options->{"fork"};
	return ();
}

sub wait {
	my ($self, $options) = @_;

	# Waiting on a PID twice can be bad because the kernel reuses PIDs, so if this
	# program forks another child, we could accidentally wait on it.
	die "Child was already waited on\n" if defined $self->{"exit_status"};

	unless($options->{"no_close"} or $options->{"nonblock"}) {
		# Close the pipes so the child receives EOF on stdin and isn't blocking
		# to write to stdout or stderr.  Ignore errors because these may
		# already be closed.
		close $self->{"stdin"} if ref $self->{"stdin"};
		close $self->{"stdout"} if ref $self->{"stdout"};
		close $self->{"stderr"} if ref $self->{"stderr"};
	}

	my $waitpid = waitpid($self->{"child_pid"}, ($options->{"nonblock"} ? &WNOHANG : 0));
	die "Child was already waited on without calling the wait method\n" if $waitpid == -1;
	return undef if $waitpid == 0;  # Child didn't exit yet.
	$self->{"exit_status"} = $?;
	warn sprintf("Exit status was %s (%s)", $?, ($? >> 8)) if $self->{"debug"};
	return 1;
}

sub DESTROY {
	my ($self) = @_;

	return if $self->{"no_autowait"};
	return unless defined $self->{"child_pid"};  # Haven't forked yet.  (Died during constructor.)
	return if defined $self->{"exit_status"};  # Already waited on the child.

	# Wait for the child so we don't accidentally leave a zombie process.
	$self->wait();
}

sub stdin { $_[0]->{"stdin"}; }
sub stdout { $_[0]->{"stdout"}; }
sub stderr { $_[0]->{"stderr"}; }
sub child_pid { $_[0]->{"child_pid"}; }
sub exit_status { $_[0]->{"exit_status"}; }

sub _set_cloexec {
	my ($fh) = @_;
	my $fcntl;
	$fcntl = fcntl($fh, F_GETFL, 0) or die "fcntl: $!\n";
	$fcntl = fcntl($fh, F_SETFL, $fcntl | FD_CLOEXEC) or die "fnctl: $!\n";
}

sub test {
	my $test = "";

	# Test case for ls | sort > /tmp/Proc-SafeExec-test1.txt
	my ($output_fh, $output_filename) = File::Temp::tempfile("Proc-SafeExec.XXXXXXXXXXXXXXXX", SUFFIX => ".txt", DIR => File::Spec->tmpdir());
	eval {
		my $ls = new Proc::SafeExec({exec => ["ls"], stdout => "new"});
		my $sort = new Proc::SafeExec({exec => ["sort"], stdin => $ls->stdout, stdout => $output_fh});
		$ls->wait() or die '$ls->wait() returned false';
		$sort->wait() or die '$sort->wait() returned false';
		$ls->exit_status and die "ls exited with status " . $ls->exit_status;
		$sort->exit_status and die "sort exited with status " . $sort->exit_status;
	};
	unlink($output_filename);
	$test .= "$@not " if $@;
	$test .= "ok - ls | sort > /tmp/Proc-SafeExec-test1.txt\n";

	# Another test case for ls | sort > /tmp/Proc-SafeExec-test2.txt
	# This one will deadlock if the parent doesn't close stdin.
	($output_fh, $output_filename) = File::Temp::tempfile("Proc-SafeExec.XXXXXXXXXXXXXXXX", SUFFIX => ".txt", DIR => File::Spec->tmpdir());
	eval {
		my $sort = new Proc::SafeExec({exec => ["sort"], stdin => "new", stdout => $output_fh});
		my $ls = new Proc::SafeExec({exec => ["ls"], stdout => $sort->stdin});
		$ls->wait() or die '$ls->wait() returned false';
		$sort->wait() or die '$sort->wait() returned false';
		$ls->exit_status and die "ls exited with status " . $ls->exit_status;
		$sort->exit_status and die "sort exited with status " . $sort->exit_status;
	};
	unlink($output_filename);
	$test .= "$@not " if $@;
	$test .= "ok - ls | sort > /tmp/Proc-SafeExec-test2.txt\n";

	# Test case for exec failure.
	my $message;
	eval {
		eval {
			my $nope = new Proc::SafeExec({exec => ["/nonexistent"]});
		};
		die "Testing exec failure should have died." unless $@;
		$message = $@;
		chomp $message;
	};
	$test .= "$@not " if $@;
	$test .= "ok - testing exec failure:  $message\n";

	return $test;
}
