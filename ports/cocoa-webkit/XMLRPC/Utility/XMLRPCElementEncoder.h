//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import <Foundation/Foundation.h>

@interface XMLRPCElementEncoder : NSObject

+ (NSString*)encodeObject:(id)object;

+ (NSString*)encodeArray:(NSArray*)array;

+ (NSString*)encodeDictionary:(NSDictionary*)dictionary;

+ (NSString*)encodeBoolean:(CFBooleanRef)boolean;

+ (NSString*)encodeNumber:(NSNumber*)number;

+ (NSString*)encodeString:(NSString*)string omitTag:(BOOL)omitTag;

+ (NSString*)encodeDate:(NSDate*)date;

+ (NSString*)encodeData:(NSData*)data;

@end
