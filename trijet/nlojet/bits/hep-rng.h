//  Copyright (C) 2002 Zoltan Nagy
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#ifndef __NLO_HEP_RNG_H__
#define __NLO_HEP_RNG_H__ 1


namespace nlo {
  
  
  /// \brief Abstract class for implementing random number generators
  class random_generator
  {
  public:
    ///   destructor
    virtual ~random_generator() {}
    
    /// Returns the minimum value that \ref random_generator::get can return
    unsigned long min() const { return _M_min;}

    /// Returns the minimum value that \ref random_generator::get can return
    unsigned long max() const { return _M_max;}
    
    /// \fn virtual void random_generator::set(unsigned long int seed) = 0;
    /// \brief This function initializes (or `seeds') the random number generator.
    ///
    ///  If the generator is seeded with the same value of seed on
    ///  two different runs, the same stream of random numbers will be
    ///  generated by successive calls to the routines below.  If different
    ///  values of S are supplied, then the generated streams of random
    ///  numbers should be completely different.  If the seed is zero
    ///  then the standard seed from the original implementation is used
    ///  instead.
    /// \param seed is an unsigned long arbitrary parameter which initialize
    ///  the random number generator.
    virtual void set(unsigned long int) = 0;

    /// \brief This function returns a random integer.
    ///
    /// The minimum and maximum values depend on the algorithm used, but 
    /// all integers in the range [MIN,MAX] are equally likely. The values of
    /// MIN and MAX can determined using the auxiliary functions
    /// \ref max() and \ref min().
    /// generate unsigned long random number
    virtual unsigned long int get() const = 0;
    
    /// \brief This function returns a double precision floating point number
    /// uniformly distributed in the range [0,1).
    ///
    /// The range includes 0.0
    /// but excludes 1.0.  The value is typically obtained by dividing the
    /// result of \ref get() by \ref max() + 1.0 in double precision.  
    /// Some generators compute this ratio internally so that
    /// they can provide floating point numbers with more than 32 bits of
    /// randomness (the maximum number of bits that can be portably
    /// represented in a single `unsigned long int').
    virtual double get_double() const = 0;
    
    /// same like \ref get_double()
    double uniform() const { return get_double();}

    /// same like \ref get_double()
    double operator()() const { return get_double();}

    /// \brief This function returns a positive double precision floating point
    /// number uniformly distributed in the range (0,1), excluding both
    /// 0.0 and 1.0.  
    ///
    /// The number is obtained by sampling the generator
    /// with the algorithm of \ref uniform() until a non-zero value is
    /// obtained.  You can use this function if you need to avoid a
    /// singularity at 0.0.
    double uniform_pos() const {
      double res;
      while((res = get_double()) == 0.0);
      return res;
    }
    
  protected:
    /// \brief Protected constructor.
    /// This constructor initializes the class with the minimum and maximum 
    /// of the random number generator.
    /// \param min is the minimum value
    /// \param max is the maximum value
    random_generator(unsigned long int min, unsigned long int max) 
      : _M_min(min), _M_max(max) {}
    
  private:
    unsigned long int _M_min, _M_max;
  };
  
}

#endif