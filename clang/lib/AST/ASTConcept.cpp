//===--- ASTConcept.cpp - Concepts Related AST Data Structures --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file defines AST data structures related to concepts.
///
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConcept.h"
#include "clang/AST/ASTContext.h"
using namespace clang;

ASTConstraintSatisfaction::ASTConstraintSatisfaction(const ASTContext &C,
    bool IsSatisfied, ArrayRef<UnsatisfiedConstraintRecord> Details):
    NumRecords{Details.size()}, IsSatisfied{IsSatisfied} {
  for (unsigned I = 0; I < NumRecords; ++I) {
    auto &Detail = Details[I];
    if (Detail.second.is<Expr *>())
      new (getTrailingObjects<ASTUnsatisfiedConstraintRecord>() + I)
          ASTUnsatisfiedConstraintRecord{
              Detail.first,
              ASTUnsatisfiedConstraintRecord::second_type(
                  Detail.second.get<Expr *>())};
    else {
      auto &SubstitutionDiag = *Detail.second.get<PartialDiagnosticAt *>();
      llvm::SmallVector<char, 128> DiagStr;
      SubstitutionDiag.second.EmitToString(C.getDiagnostics(), DiagStr);
      unsigned MessageSize = DiagStr.size();
      char *Mem = new (C) char[MessageSize];
      memcpy(Mem, DiagStr.data(), MessageSize);
      auto *NewSubstDiag = new (C) std::pair<SourceLocation, StringRef>(
          SubstitutionDiag.first, StringRef(Mem, MessageSize));
      new (getTrailingObjects<ASTUnsatisfiedConstraintRecord>() + I)
         ASTUnsatisfiedConstraintRecord{
            Detail.first,
            ASTUnsatisfiedConstraintRecord::second_type(NewSubstDiag)};
    }
  }
}


ASTConstraintSatisfaction *
ASTConstraintSatisfaction::Create(const ASTContext &C, bool IsSatisfied,
    ArrayRef<UnsatisfiedConstraintRecord> Details) {
  std::size_t size =
      totalSizeToAlloc<ASTUnsatisfiedConstraintRecord>(Details.size());
  void *Mem = C.Allocate(size, alignof(ASTConstraintSatisfaction));
  return new (Mem) ASTConstraintSatisfaction(C, IsSatisfied, Details);
}

ASTConstraintSatisfaction::ASTConstraintSatisfaction(bool IsSatisfied,
                                                     unsigned NumRecords):
    NumRecords{NumRecords}, IsSatisfied{IsSatisfied} { }


ASTConstraintSatisfaction *
ASTConstraintSatisfaction::Create(const ASTContext &C, bool IsSatisfied,
                                  unsigned NumRecords) {
  std::size_t size =
      totalSizeToAlloc<ASTUnsatisfiedConstraintRecord>(NumRecords);
  void *Mem = C.Allocate(size, alignof(ASTConstraintSatisfaction));
  return new (Mem) ASTConstraintSatisfaction(IsSatisfied, NumRecords);
}
