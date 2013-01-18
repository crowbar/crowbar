# Crowbar User Guide

This is an overview about crowbar

## Introduction

This document provides instructions you for operating Crowbar.  Please refer to additional user guide for specific products that are deployed by Crowbar such as OpenStack™ or Apache™ Hadoop™.   
Concepts

The purpose of this guide is to explain the user interface of Crowbar.  Use the Crowbar User Guide for assistance with installing Crowbar and configuring the target system.

Note: Concepts beyond the scope of this guide are introduced as needed in notes and references to other documentation.
Opscode Chef Server

Crowbar makes extensive use of Opscode Chef Server, http://opscode.com. To explain Crowbar actions, you should understand the underlying Chef implementation.  
To use Crowbar, it is not necessary to log into the Chef Serversll; consequently, use of the Chef UI is not covered in this guide.  Supplemental information about Chef is included.

This guide provides this additional Chef information as notes flagged with the Opscode logo.  
Dell Specific Options

The Dell EULA version of Crowbar provides additional functionality and color pallets than the open source version.  When divergences are relevant, they are identified.
To perform some configuration options and provide some integration, we use libraries that cannot be distributed using open source.

Crowbar is not limited to managing Dell servers and components.  Due to driver requirements, some barclamps, for example: BIOS & RAID, must be targeted to specific hardware; however, those barclamps are not required for system configuration.

