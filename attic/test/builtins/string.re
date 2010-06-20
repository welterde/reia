#
# StringTest: Tests for Reia's string type
# Copyright (C)2009 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module StringTest
  def run
    [
      length_test(), 
      inspect_test(), 
      sub_test(),
      interpolation_test(),
      #split_test(), -- requires Erlang R12B-5, which isn't generally available :/
      to_constant_test()
    ]
  end
  
  def length_test
    TestHelper.expect(Str, "knows its length") do
      (6, "foobar".length())
    end
  end
  
  def inspect_test
    TestHelper.expect(Str, "inspects properly") do
      ("\"foobar\"", "foobar".inspect())
    end
  end
    
  def sub_test
    TestHelper.expect(Str, "substitutes properly") do
      ("bazbar", "foobar".sub(/foo/, "baz"))
    end
  end
    
  def interpolation_test
    (foo, bar) = (1, 2)
    TestHelper.expect(Str, "interpolates nested Reia syntax") do
      ("foo: 1, bar: 2", "foo: #{foo}, bar: #{bar}")
    end
  end
    
  def split_test
    TestHelper.expect(Str, "splits properly") do
      (["foo", "bar", "baz"], "foo    bar baz".split(/\s+/))
    end
  end
  
  def to_constant_test
    TestHelper.expect(Str, "converts to constant") do
      (Object, "Object".to_constant())
    end
  end
end