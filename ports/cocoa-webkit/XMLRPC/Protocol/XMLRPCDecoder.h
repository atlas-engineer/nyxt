//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import <Foundation/Foundation.h>

@protocol XMLRPCDecoder <NSObject>

- (id)decodeWithData:(NSData*) data;

- (NSString *)method;

- (NSArray *)parameters;

@end
