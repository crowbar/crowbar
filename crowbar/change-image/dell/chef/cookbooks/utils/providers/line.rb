
action :add do
  filename = "/tmp/#{new_resource.file.gsub("/","_")}.lock"
  f = ::File.new(filename, ::File::RDWR|::File::CREAT, 0644)
  rc = false
  count = 0
  while rc == false do
    count = count + 1
    rc = f.flock(::File::LOCK_EX|::File::LOCK_NB)
    sleep 1 if rc == false
  end

  system "/bin/egrep -q '^#{new_resource.name}$' '#{new_resource.file}'"
  ret=$?
  if ret != 0
    b = bash "add #{new_resource.name}" do
      code "/bin/echo '#{new_resource.name}' >> '#{new_resource.file}'"
    end
    b.action(:run)
    new_resource.updated_by_last_action(true)
  end

  f.flock(::File::LOCK_UN)
  f.close
end

action :remove do
  filename = "/tmp/#{new_resource.file.gsub("/","_")}.lock"
  f = ::File.new(filename, ::File::RDWR|::File::CREAT, 0644)
  rc = false
  count = 0
  while rc == false do
    count = count + 1
    rc = f.flock(::File::LOCK_EX|::File::LOCK_NB)
    sleep 1 if rc == false
  end

  system "/bin/egrep -q '^#{new_resource.name}$' '#{new_resource.file}'"
  ret=$?
  if ret == 0
    line_str = new_resource.name.gsub("\"", "\\\"").gsub("/", "\\/").gsub("'", "\\'")
    b = bash "remove #{new_resource.name}" do
      code "/usr/bin/perl -ni -e 'print unless /^#{line_str}$/' '#{new_resource.file}'"
    end
    b.action(:run)
    new_resource.updated_by_last_action(true)
  end

  f.flock(::File::LOCK_UN)
  f.close
end

