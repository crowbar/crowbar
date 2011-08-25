# Be sure to restart your server when you modify this file.

# Your secret key for verifying cookie session data integrity.
# If you change this key, all old sessions will become invalid!
# Make sure the secret is at least 30 characters and all random, 
# no regular words or you'll be exposed to dictionary attacks.
ActionController::Base.session = {
  :key         => '_crowbar_framework_session',
  :secret      => 'ae66894fcee606d20eab9637a28684a8e9a12d36a91d075035e5703ed49bb26a0f9163fd0954185dea2b701cf79cefad152fb6da0075af43066b790707f52424'
}

# Use the database for sessions instead of the cookie-based default,
# which shouldn't be used to store highly confidential information
# (create the session table with "rake db:sessions:create")
# ActionController::Base.session_store = :active_record_store
