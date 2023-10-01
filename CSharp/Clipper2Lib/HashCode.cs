using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Security.Cryptography;

namespace Clipper2Lib
{
  /*

  Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.

  The xxHash32 implementation is based on the code published by Yann Collet:
  https://raw.githubusercontent.com/Cyan4973/xxHash/5c174cfa4e45a42f94082dc0d4539b39696afea1/xxhash.c

    xxHash - Fast Hash algorithm
    Copyright (C) 2012-2016, Yann Collet

    BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:

    * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
    copyright notice, this list of conditions and the following disclaimer
    in the documentation and/or other materials provided with the
    distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    You can contact the author at :
    - xxHash homepage: http://www.xxhash.com
    - xxHash source repository : https://github.com/Cyan4973/xxHash
  */

  public struct HashCode
  {
    private static readonly uint s_seed = GenerateGlobalSeed();

    private const uint Prime1 = 2654435761U;
    private const uint Prime2 = 2246822519U;
    private const uint Prime3 = 3266489917U;
    private const uint Prime4 = 668265263U;
    private const uint Prime5 = 374761393U;

    private static uint GenerateGlobalSeed()
    {
      using RandomNumberGenerator randomNumberGenerator = RandomNumberGenerator.Create();
      byte[] data = new byte[sizeof(uint)];
      randomNumberGenerator.GetBytes(data);
      return BitConverter.ToUInt32(data, 0);
    }

    public static int Combine<T1, T2>(T1 value1, T2 value2)
    {
      uint hc1 = (uint) (value1?.GetHashCode() ?? 0);
      uint hc2 = (uint) (value2?.GetHashCode() ?? 0);

      uint hash = MixEmptyState();
      hash += 8;

      hash = QueueRound(hash, hc1);
      hash = QueueRound(hash, hc2);

      hash = MixFinal(hash);
      return (int) hash;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static uint QueueRound(uint hash, uint queuedValue)
    {
      return RotateLeft(hash + (queuedValue * Prime3), 17) * Prime4;
    }

    private static uint MixEmptyState()
    {
      return s_seed + Prime5;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static uint MixFinal(uint hash)
    {
      hash ^= hash >> 15;
      hash *= Prime2;
      hash ^= hash >> 13;
      hash *= Prime3;
      hash ^= hash >> 16;
      return hash;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static uint RotateLeft(uint value, int offset)
    {
      return (value << offset) | (value >> (32 - offset));
    }

#pragma warning disable CS0809 // Obsolete member overrides non-obsolete member
    [Obsolete("HashCode is a mutable struct and should not be compared with other HashCodes. Use ToHashCode to retrieve the computed hash code.", error: true)]
    [EditorBrowsable(EditorBrowsableState.Never)]
    public override int GetHashCode()
    {
      throw new NotSupportedException($"{nameof(HashCode)}.{nameof(GetHashCode)}() is not supported");
    }

    [Obsolete("HashCode is a mutable struct and should not be compared with other HashCodes.", error: true)]
    [EditorBrowsable(EditorBrowsableState.Never)]
    public override bool Equals(object? obj)
    {
      throw new NotSupportedException($"{nameof(HashCode)}.{nameof(Equals)}() is not supported");
    }
#pragma warning restore CS0809 // Obsolete member overrides non-obsolete member
  }
}