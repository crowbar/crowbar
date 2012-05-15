#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2008 Opscode, Inc.
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

require 'ohai/exception'
require 'ohai/config'
require 'ohai/log'
require 'stringio'
require 'tmpdir'
require 'fcntl'
require 'etc'
require 'systemu'

module Ohai
  module Mixin
    module Command

      def run_command(args={})
        if args.has_key?(:creates)
          if File.exists?(args[:creates])
            Ohai::Log.debug("Skipping #{args[:command]} - creates #{args[:creates]} exists.")
            return false
          end
        end

        stdout_string = nil
        stderr_string = nil

        args[:cwd] ||= Dir.tmpdir
        unless File.directory?(args[:cwd])
          raise Ohai::Exceptions::Exec, "#{args[:cwd]} does not exist or is not a directory"
        end

        status = nil
        Dir.chdir(args[:cwd]) do
          status, stdout_string, stderr_string = run_command_backend(args[:command], args[:timeout])
          # systemu returns 42 when it hits unexpected errors
          if status.exitstatus == 42 and stderr_string == ""
            stderr_string = "Failed to run: #{args[:command]}, assuming command not found"
            Ohai::Log.debug(stderr_string)
          end

          if stdout_string
            Ohai::Log.debug("---- Begin #{args[:command]} STDOUT ----")
            Ohai::Log.debug(stdout_string.strip)
            Ohai::Log.debug("---- End #{args[:command]} STDOUT ----")
          end
          if stderr_string
            Ohai::Log.debug("---- Begin #{args[:command]} STDERR ----")
            Ohai::Log.debug(stderr_string.strip)
            Ohai::Log.debug("---- End #{args[:command]} STDERR ----")
          end

          args[:returns] ||= 0
          args[:no_status_check] ||= false
          if status.exitstatus != args[:returns] and not args[:no_status_check]
            raise Ohai::Exceptions::Exec, "#{args[:command_string]} returned #{status.exitstatus}, expected #{args[:returns]}"
          else
            Ohai::Log.debug("Ran #{args[:command_string]} (#{args[:command]}) returned #{status.exitstatus}")
          end
        end
        return status, stdout_string, stderr_string
      end

      module_function :run_command

      def run_command_unix(command, timeout)
        stderr_string, stdout_string, status = "", "", nil

        exec_processing_block = lambda do |pid, stdin, stdout, stderr|
          stdout_string, stderr_string = stdout.string.chomp, stderr.string.chomp
        end

        if timeout
          begin
            Timeout.timeout(timeout) do
              status = popen4(command, {}, &exec_processing_block)
            end
          rescue Timeout::Error => e
            Chef::Log.error("#{command} exceeded timeout #{timeout}")
            raise(e)
          end
        else
          status = popen4(command, {}, &exec_processing_block)
        end
        return status, stdout_string, stderr_string
      end

      def run_command_windows(command, timeout)
        if timeout
          begin
            systemu(command)
          rescue SystemExit => e
            raise
          rescue Timeout::Error => e
            Ohai::Log.error("#{command} exceeded timeout #{timeout}")
            raise(e)
          end
        else
          systemu(command)
        end
      end

      if RUBY_PLATFORM =~ /mswin|mingw32|windows/
        alias :run_command_backend :run_command_windows
      else
        alias :run_command_backend :run_command_unix
      end
      # This is taken directly from Ara T Howard's Open4 library, and then
      # modified to suit the needs of Ohai.  Any bugs here are most likely
      # my own, and not Ara's.
      #
      # The original appears in external/open4.rb in its unmodified form.
      #
      # Thanks Ara!
      def popen4(cmd, args={}, &b)
        GC.disable

        # Waitlast - this is magic.
        #
        # Do we wait for the child process to die before we yield
        # to the block, or after?  That is the magic of waitlast.
        #
        # By default, we are waiting before we yield the block.
        args[:waitlast] ||= false

        args[:user] ||= nil
        unless args[:user].kind_of?(Integer)
          args[:user] = Etc.getpwnam(args[:user]).uid if args[:user]
        end
        args[:group] ||= nil
        unless args[:group].kind_of?(Integer)
          args[:group] = Etc.getgrnam(args[:group]).gid if args[:group]
        end
        args[:environment] ||= {}

        # Default on C locale so parsing commands output can be done
        # independently of the node's default locale.
        # "LC_ALL" could be set to nil, in which case we also must ignore it.
        unless args[:environment].has_key?("LC_ALL")
          args[:environment]["LC_ALL"] = "C"
        end

        pw, pr, pe, ps = IO.pipe, IO.pipe, IO.pipe, IO.pipe

        verbose = $VERBOSE
        begin
          $VERBOSE = nil
          ps.last.fcntl(Fcntl::F_SETFD, Fcntl::FD_CLOEXEC)

          cid = fork {
            Process.setsid

            pw.last.close
            STDIN.reopen pw.first
            pw.first.close

            pr.first.close
            STDOUT.reopen pr.last
            pr.last.close

            pe.first.close
            STDERR.reopen pe.last
            pe.last.close

            STDOUT.sync = STDERR.sync = true

            if args[:group]
              Process.egid = args[:group]
              Process.gid = args[:group]
            end

            if args[:user]
              Process.euid = args[:user]
              Process.uid = args[:user]
            end

            args[:environment].each do |key,value|
              ENV[key] = value
            end

            if args[:umask]
              umask = ((args[:umask].respond_to?(:oct) ? args[:umask].oct : args[:umask].to_i) & 007777)
              File.umask(umask)
            end

            begin
              if cmd.kind_of?(Array)
                exec(*cmd)
              else
                exec(cmd)
              end
              raise 'forty-two'
            rescue Exception => e
              Marshal.dump(e, ps.last)
              ps.last.flush
            end
            ps.last.close unless (ps.last.closed?)
            exit!
          }
        ensure
          $VERBOSE = verbose
        end

        [pw.first, pr.last, pe.last, ps.last].each{|fd| fd.close}

        begin
          e = Marshal.load ps.first
          raise(Exception === e ? e : "unknown failure!")
        rescue EOFError # If we get an EOF error, then the exec was successful
          42
        ensure
          ps.first.close
        end

        pw.last.sync = true

        pi = [pw.last, pr.first, pe.first]

        if b
          begin
            if args[:waitlast]
              b[cid, *pi]
              # send EOF so that if the child process is reading from STDIN
              # it will actually finish up and exit
              pi[0].close_write
              Process.waitpid2(cid).last
            else
              # This took some doing.
              # The trick here is to close STDIN
              # Then set our end of the childs pipes to be O_NONBLOCK
              # Then wait for the child to die, which means any IO it
              # wants to do must be done - it's dead.  If it isn't,
              # it's because something totally skanky is happening,
              # and we don't care.
              o = StringIO.new
              e = StringIO.new

              #pi[0].close

              stdout = pi[1]
              stderr = pi[2]

              stdout.sync = true
              stderr.sync = true

              stdout.fcntl(Fcntl::F_SETFL, pi[1].fcntl(Fcntl::F_GETFL) | Fcntl::O_NONBLOCK)
              stderr.fcntl(Fcntl::F_SETFL, pi[2].fcntl(Fcntl::F_GETFL) | Fcntl::O_NONBLOCK)

              stdout_finished = false
              stderr_finished = false

              results = nil

              while !stdout_finished || !stderr_finished
                begin
                  channels_to_watch = []
                  channels_to_watch << stdout if !stdout_finished
                  channels_to_watch << stderr if !stderr_finished
                  ready = IO.select(channels_to_watch, nil, nil, 1.0)
                rescue Errno::EAGAIN
                ensure
                  results = Process.waitpid2(cid, Process::WNOHANG)
                  if results
                    stdout_finished = true
                    stderr_finished = true
                  end
                end

                if ready && ready.first.include?(stdout)
                  line = results ? stdout.gets(nil) : stdout.gets
                  if line
                    o.write(line)
                  else
                    stdout_finished = true
                  end
                end
                if ready && ready.first.include?(stderr)
                  line = results ? stderr.gets(nil) : stderr.gets
                  if line
                    e.write(line)
                  else
                    stderr_finished = true
                  end
                end
              end
              results = Process.waitpid2(cid) unless results
              o.rewind
              e.rewind

              # **OHAI-275**
              # The way we read from the pipes causes ruby to mark the strings
              # as ASCII-8BIT (i.e., binary), but the content should be encoded
              # as the default external encoding. For example, a command may
              # return data encoded as UTF-8, but the strings will be marked as
              # ASCII-8BIT. Later, when you attempt to print the values as
              # UTF-8, Ruby will try to convert them and fail, raising an
              # error.
              #
              # Ruby always marks strings as binary when read from IO in
              # incomplete chunks, since you may have split the data within a
              # multibyte char. In our case, we concat the chunks back
              # together, so any multibyte chars will be reassembled.
              #
              # Note that all of this applies only to Ruby 1.9, which we check
              # for by making sure that the Encoding class exists and strings
              # have encoding methods.
              if "".respond_to?(:force_encoding) && defined?(Encoding)
                o.string.force_encoding(Encoding.default_external)
                e.string.force_encoding(Encoding.default_external)
              end

              b[cid, pi[0], o, e]
              results.last
            end
          ensure
            pi.each{|fd| fd.close unless fd.closed?}
          end
        else
          [cid, pw.last, pr.first, pe.first]
        end
      rescue Errno::ENOENT
        raise Ohai::Exceptions::Exec, "command #{cmd} doesn't exist or is not in the PATH"
      ensure
        GC.enable
      end

      module_function :popen4
    end
  end
end
