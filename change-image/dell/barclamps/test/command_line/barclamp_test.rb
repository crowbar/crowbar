#!/usr/bin/env ruby

require File.join(File.expand_path(File.dirname(__FILE__)), "barclamp_lib")
@barclamp = "test"
@testcase = "none"

@allow_zero_args = true
@t_debug = false
@t_exit = false
@rc = 0

# Handle extra arguments
def handle_test_debug
  @t_debug = true
end

def handle_test_exit
  @t_exit = true
end


@options << [ [ '--test_debug', '-t', GetoptLong::NO_ARGUMENT ], "--test_debug or -t to turn on debug of test", "handle_test_debug" ]
@options << [ [ '--test_exit', '-e', GetoptLong::NO_ARGUMENT ], "--test_exit or -e to exit on failure", "handle_test_exit" ]
opt_parse


def assertHashAttribute(hash, field, expected, emessage, smessage)
  data = hash
  fields = field.split(".") 
  fields.each { |x| data = data.nil? ? nil : data[x] }
  assertEqual(data, expected, emessage, smessage)
end

def assertEqualTimes(valueF, expected, emessage, smessage, times)
  count = 0
  while count < times 
    value = eval(valueF)
    if value == expected
      puts "#{@testcase}: #{smessage}" if @t_debug
      return true
    end
    sleep 1
    count += 1
  end
  puts "#{@testcase}: Value: #{value.inspect}  Expected: #{expected.inspect}: #{emessage}"
  @rc = @rc + 1

  exit if @t_exit
  false
end


def assertEqual(value, expected, emessage, smessage)
  if value != expected
    puts "#{@testcase}: Value: #{value.inspect}  Expected: #{expected.inspect}: #{emessage}"
    @rc = @rc + 1
    exit if @t_exit
  else
    puts "#{@testcase}: #{smessage}" if @t_debug
  end
  value == expected
end


def empty_db_tests
  assertEqual(get_json("/"), [[], 200], "Get of list should be an empty list", "Success: empty list returned for /")
  assertEqual(get_json("/proposals"), [[], 200], "Get of proposal should be an empty list",
              "Success: empty list returned for /proposals")
  assertEqual(list()[0], "No current configurations", "CLI: Get of list should be empty", "Success: cli returned empty list for /")
  assertEqual(proposal_list()[0], "No current proposals", "CLI: Get of list should be empty",
              "Success: cli returned empty list for /proposals")
end


#
# This currently only does API testing
#
def api_tests
  @testcase = "apt_test"
  empty_db_tests

  data = { "id" => "test_p1" }.to_json
  assertEqual(put_json("/proposals", data), ["{}", 200], "Failed to API create proposal: test_p1", "Success: create test_p1 proposal")

  assertEqual(put_json("/proposals/fred", data)[1], 405, "Failed to API create with bad URL proposal: test_p1", "Success: create test_p1 proposal with bad url")

  assertEqualTimes("get_json(\"/proposals\")", [["test_p1"], 200], "Failed to API get proposals: test_p1", "Success: list of proposals with test_p1", 60)

  response = get_json("/proposals/test_p1")
  assertEqual(response[1], 200, "Failed to show test_p1", "Success: getting test_p1")

  assertEqual(post_json("/proposals/test_p1", response[0].to_json), ["{}", 200], "Failed to edit test_p1", "Success: editting test_p1")
  
  response[0][:fred] = "bad"
  assertEqual(post_json("/proposals/test_p1", response[0].to_json), ["key 'fred:' is undefined.\n", 400], "Failed to fail editting test_p1", "Success: failed to edit test_p1")

  assertEqual(delete_json("/proposals/test_p1"), ["{}", 200], "Failed to API delete proposal: test_p1", "Success: delete test_p1 proposal")
  assertEqual(delete_json("/proposals/test_p1"), ["", 404], "Failed to API delete proposal: missing test_p1", "Success: not finding to delete test_p1 proposal")

  data = { "id" => "test_p2" }.to_json
  assertEqual(put_json("/proposals", data), ["{}", 200], "Failed to API create proposal: test_p2", "Success: create test_p2 proposal")
  assertEqual(post_json("/proposals/commit/test_p2", ""), ["{}", 200], "Failed to commit test_p2", "Success: created committing test_p2")

  assertEqualTimes("get_json(\"/proposals\")", [["test_p2"], 200], "Failed to API get proposals: test_p2", "Success: list of proposals with test_p2", 60)
  assertEqualTimes("get_json(\"/\")", [["test_p2"], 200], "Failed to API get: test_p2", "Success: list with test_p2", 60)

  response = get_json("/test_p2")
  assertEqual(response[1], 200, "Failed to show test_p2", "Success: getting test_p2")

  assertEqual(delete_json("/test_p2"), ["{}", 200], "Failed to API delete: test_p2", "Success: delete test_p2")
  assertEqual(delete_json("/test_p2"), ["", 404], "Failed to API delete: missing test_p2", "Success: delete missing test_p2")

  assertEqual(delete_json("/proposals/test_p2"), ["{}", 200], "Failed to API delete proposal: test_p2", "Success: delete test_p2 proposal")

  empty_db_tests
end


def transition_machine (name, state)
  oldbc = @barclamp
  @barclamp = "crowbar"
  data = { "name" => name, "state" => state }.to_json
  answer = post_json("/transition/default", data)
  assertEqual(answer[1], 200, "Failed to transition #{name}: name test", "Success transition #{name}: result test")
  assertEqual(JSON.parse(answer[0])["name"], name, "Failed to transition #{name}: name test", "Success transition #{name}: name test")

  @barclamp = "machines"
  assertEqualTimes("get_json(\"/\")[0].include? \"#{name}\"", true, "Failed to find #{name} in node list", "Success: found #{name} in node list", 60)

  @barclamp = oldbc
end


# Node Manipulation tests
def node_manipulation
  @testcase = "node_manipulate"
  @barclamp = "test"

  empty_db_tests

  transition_machine "dtest-machine-1.dell.com", "testing"

  data = { "id" => "test_p1" }.to_json
  assertEqual(put_json("/proposals", data), ["{}", 200], "Failed to API create proposal: test_p1", "Success: create test_p1 proposal")
  response = get_json("/proposals/test_p1")
  assertHashAttribute(response[0], "deployment.test.elements.test-single", [ "dtest-machine-1.dell.com" ], "Failed to add dtest-machine-1.dell.com to proposal", "Success creating proposal with test machine single")
  assertHashAttribute(response[0], "deployment.test.elements.test-multi-head", nil, "Failed with values in test-multi-head to proposal", "Success creating proposal without test-multi-head")
  assertHashAttribute(response[0], "deployment.test.elements.test-multi-rest", nil, "Failed with values in test-multi-head to proposal", "Success creating proposal without test-multi-rest")

  transition_machine "dtest-machine-2.dell.com", "testing"
 
  assertEqual(post_json("/proposals/commit/test_p1", ""), ["[\"dtest-machine-1.dell.com\"]", 202], "Failed to queue commit test_p1", "Success: queued committing test_p1")

  response = get_json("/test_p1")
  assertEqual(response[1], 404, "Found test_p1", "Success: not finding active test_p1")

  transition_machine "dtest-machine-1.dell.com", "ready"

  response = get_json("/test_p1")
  assertEqual(response[1], 200, "Didn't find test_p1", "Success: found active test_p1")

  response = get_json("/test_p1")
  assertHashAttribute(response[0], "deployment.test.elements.test-single", [ "dtest-machine-1.dell.com" ], "Failed to add dtest-machine-1.dell.com to role", "Success creating role with test machine single")
  assertHashAttribute(response[0], "deployment.test.elements.test-multi-head", nil, "Failed with values in test-multi-head to role", "Success creating role without test-multi-head")
  assertHashAttribute(response[0], "deployment.test.elements.test-multi-rest", nil, "Failed with values in test-multi-head to role", "Success creating role without test-multi-rest")
  
  # GREG: test machines have roles.

  assertEqual(delete_json("/proposals/test_p1"), ["{}", 200], "Failed to delete proposal: test_p1", "Success: delete test_p1 proposal")
  assertEqual(delete_json("/test_p1"), ["{}", 200], "Failed to delete active: test_p1", "Success: delete test_p1 active")

  # GREG: Check that machines don't have roles.

  empty_db_tests

  # Test multiple nodes
  data = { "id" => "test_p1" }.to_json
  assertEqual(put_json("/proposals", data), ["{}", 200], "Failed to API create proposal: test_p1", "Success: create test_p1 proposal")
  response = get_json("/proposals/test_p1")
  assertHashAttribute(response[0], "deployment.test.elements.test-single", nil, "Failed with values in test-single to role", "Success creating role without test machine single")
  assertHashAttribute(response[0], "deployment.test.elements.test-multi-head", [ "dtest-machine-1.dell.com" ], "Failed to add dtest-machine-1.dell.com to proposal", "Success creating proposal with test-multi-head")
  assertHashAttribute(response[0], "deployment.test.elements.test-multi-rest", [ "dtest-machine-2.dell.com" ], "Failed to add dtest-machine-2.dell.com to proposal", "Success creating proposal with test-multi-rest")

  assertEqual(delete_json("/proposals/test_p1"), ["{}", 200], "Failed to delete proposal: test_p1", "Success: delete test_p1 proposal")

#
# Delete only works if the deployer runs.
#  oldbc = @barclamp
#  @barclamp = "machines"
#  assertEqual(delete_json("?name=dtest-machine-1.dell.com"), ["{}", 200], "Failed to delete machine: dtest-machine-1.dell.com", "Success: deleted dtest-machine-1.dell.com")
#  assertEqual(delete_json("?name=dtest-machine-2.dell.com"), ["{}", 200], "Failed to delete machine: dtest-machine-2.dell.com", "Success: deleted dtest-machine-2.dell.com")
#
#  assertEqualTimes("get_json(\"/\")", ["{}", 200], "Failed: Nodes still listed", "Success: node list is empty", 60)
#
#  @barclamp = oldbc

  empty_db_tests

  system("knife node -y delete dtest-machine-1.dell.com")
  system("knife role -y delete crowbar-dtest-machine-1_dell_com")
  system("knife node -y delete dtest-machine-2.dell.com")
  system("knife role -y delete crowbar-dtest-machine-2_dell_com")
end


# API Tests
api_tests

# Node Manipulation tests
node_manipulation

exit @rc

