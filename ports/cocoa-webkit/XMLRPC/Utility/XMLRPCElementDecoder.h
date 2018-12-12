//
// Copyright (C) 2012 Eric Czarny.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <Foundation/Foundation.h>

@interface XMLRPCElementDecoder : NSObject

+ (NSDate*)parseDateString:(NSString*)dateString withFormat:(NSString*)format;

+ (NSNumber*)parseInteger:(NSString*)value;

+ (NSNumber*)parseDouble:(NSString*)value;

+ (NSNumber*)parseBoolean:(NSString*)value;

+ (NSString*)parseString:(NSString*)value;

+ (NSDate*)parseDate:(NSString*)value;

+ (NSData*)parseData:(NSString*)value;

@end
