Feature: Manage nodelists
  In order to view the nodes in the system
  The system operator, Oscar
  wants to see all the nodes and their current status
    
  Scenario: Node list should have an "admin" node
    When I am on the nodes page
    Then I should see "admin"
    
  Scenario: Viewing node specific information in light box
    When I am on node "admin" page
    Then I should see "State"
      And I should see "Ready"
      And I should see "Address"
      And I should see "admin.pod.cloud.openstack.org"
      
  Scenario: Getting list of nodes
    When I am on the node list page
    Then the "count" entry should be at least "1"
    Then the "nodes":"admin" entry should be "ready"
    
  Scenario: Find a specific node
    Given there is a node called "CUKETEST-node-test"
    When I am on the nodes page
      Then I should see "cuketest-node-test"
      
  Scenario: Position node into rack
    Given there is a node called "CUKETEST-position"
    When I position "cuketest-position" in rack "test" u "30" slot "1"
      Then I should find "cuketest-position" nested within "test:30:1"