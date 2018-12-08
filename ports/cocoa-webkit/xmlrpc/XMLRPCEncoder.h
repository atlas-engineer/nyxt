//
// Copyright (C) 2012 Eric Czarny <eczarny@gmail.com>
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <Foundation/Foundation.h>

@protocol XMLRPCEncoder <NSObject>

- (NSString *)encode;

- (void)setMethod: (NSString *)method withParameters: (NSArray *)parameters;

- (NSString *)method;

- (NSArray *)parameters;

@end
