//
// Copyright (C) 2012 Eric Czarny <eczarny@gmail.com>
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "XMLRPCElementEncoder.h"
#import "NSData+Base64.h"
#import "NSStringAdditions.h"

@implementation XMLRPCElementEncoder

+ (NSString*)valueTag:(NSString*)tag value:(NSString*)value {
    return [NSString stringWithFormat:@"<value><%@>%@</%@></value>", tag, value, tag];
}

+ (NSString*)encodeObject:(id)object {
    if (!object) {
        return nil;
    }
    
    if ([object isKindOfClass:[NSArray class]]) {
        return [XMLRPCElementEncoder encodeArray:object];
    } else if ([object isKindOfClass:[NSDictionary class]]) {
        return [XMLRPCElementEncoder encodeDictionary:object];
    } else if (((__bridge CFBooleanRef)object == kCFBooleanTrue) || ((__bridge CFBooleanRef)object == kCFBooleanFalse)) {
        return [XMLRPCElementEncoder encodeBoolean:(CFBooleanRef)object];
    } else if ([object isKindOfClass:[NSNumber class]]) {
        return [XMLRPCElementEncoder encodeNumber:object];
    } else if ([object isKindOfClass:[NSString class]]) {
        return [XMLRPCElementEncoder encodeString:object omitTag:NO];
    } else if ([object isKindOfClass:[NSDate class]]) {
        return [XMLRPCElementEncoder encodeDate:object];
    } else if ([object isKindOfClass:[NSData class]]) {
        return [XMLRPCElementEncoder encodeData:object];
    } else {
        return [XMLRPCElementEncoder encodeString:object omitTag:NO];
    }
}

+ (NSString*)encodeArray:(NSArray*)array {
    NSMutableString* buffer = [NSMutableString string];
    NSEnumerator* enumerator = [array objectEnumerator];
    
    [buffer appendString:@"<value><array><data>"];
    
    id object = nil;
    
    while (object = [enumerator nextObject]) {
        [buffer appendString:[self encodeObject:object]];
    }
    
    [buffer appendString:@"</data></array></value>"];
    
    return (NSString*)buffer;
}

+ (NSString*)encodeDictionary:(NSDictionary*)dictionary {
    NSMutableString* buffer = [NSMutableString string];
    NSEnumerator* enumerator = [dictionary keyEnumerator];
    
    [buffer appendString:@"<value><struct>"];
    
    NSString* key = nil;
    NSObject* val;
    
    while (key = [enumerator nextObject]) {
        [buffer appendString:@"<member>"];
        [buffer appendFormat:@"<name>%@</name>", [self encodeString:key omitTag:YES]];
        
        val = [dictionary objectForKey:key];
        if (val != [NSNull null]) {
            [buffer appendString:[self encodeObject:val]];
        } else {
            [buffer appendString:@"<value><nil/></value>"];
        }
        
        [buffer appendString:@"</member>"];
    }
    
    [buffer appendString:@"</struct></value>"];
    
    return (NSString*)buffer;
}

+ (NSString*)encodeBoolean:(CFBooleanRef)boolean {
    if (boolean == kCFBooleanTrue) {
        return [self valueTag:@"boolean" value:@"1"];
    } else {
        return [self valueTag:@"boolean" value:@"0"];
    }
}

+ (NSString*)encodeNumber:(NSNumber*)number {
    NSString* numberType = [NSString stringWithCString:[number objCType] encoding:NSUTF8StringEncoding];
    
    if ([numberType isEqualToString:@"d"]) {
        return [XMLRPCElementEncoder valueTag:@"double" value:[number stringValue]];
    } else {
        return [XMLRPCElementEncoder valueTag:@"i4" value:[number stringValue]];
    }
}

+ (NSString*)encodeString:(NSString*)string omitTag:(BOOL)omitTag {
    return omitTag ? [string escapedString] : [self valueTag:@"string" value:[string escapedString]];
}

+ (NSString*)encodeDate:(NSDate*)date {
    unsigned components = kCFCalendarUnitYear | kCFCalendarUnitMonth | kCFCalendarUnitDay | kCFCalendarUnitHour | kCFCalendarUnitMinute | kCFCalendarUnitSecond;
    NSDateComponents* dateComponents = [[NSCalendar currentCalendar] components:components fromDate:date];
    NSString* buffer = [NSString stringWithFormat:@"%.4ld%.2ld%.2ldT%.2ld:%.2ld:%.2ld", (long)[dateComponents year], (long)[dateComponents month], (long)[dateComponents day], (long)[dateComponents hour], (long)[dateComponents minute], (long)[dateComponents second], nil];
    
    return [XMLRPCElementEncoder valueTag:@"dateTime.iso8601" value:buffer];
}

+ (NSString*)encodeData:(NSData*)data {
    return [XMLRPCElementEncoder valueTag:@"base64" value:[data base64EncodedString]];
}

@end
