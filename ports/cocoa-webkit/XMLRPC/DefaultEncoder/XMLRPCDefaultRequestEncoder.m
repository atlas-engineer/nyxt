//
// Copyright (C) 2012 Eric Czarny <eczarny@gmail.com>
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "XMLRPCDefaultRequestEncoder.h"
#import "NSStringAdditions.h"
#import "NSData+Base64.h"

@interface XMLRPCDefaultRequestEncoder (XMLRPCEncoderPrivate)

- (NSString *)valueTag: (NSString *)tag value: (NSString *)value;

- (NSString *)replaceTarget: (NSString *)target withValue: (NSString *)value inString: (NSString *)string;

- (NSString *)encodeObject: (id)object;

- (NSString *)encodeArray: (NSArray *)array;

- (NSString *)encodeDictionary: (NSDictionary *)dictionary;

- (NSString *)encodeBoolean: (CFBooleanRef)boolean;

- (NSString *)encodeNumber: (NSNumber *)number;

- (NSString *)encodeString: (NSString *)string omitTag: (BOOL)omitTag;

- (NSString *)encodeDate: (NSDate *)date;

- (NSString *)encodeData: (NSData *)data;

@end

@implementation XMLRPCDefaultRequestEncoder

- (id)init {
    if (self = [super init]) {
        myMethod = [[NSString alloc] init];
        myParameters = [[NSArray alloc] init];
    }
    
    return self;
}

- (NSString *)encode {
    NSMutableString *buffer = [NSMutableString stringWithString: @"<?xml version=\"1.0\"?><methodCall>"];
    
    [buffer appendFormat: @"<methodName>%@</methodName>", [self encodeString: myMethod omitTag: YES]];
    
    [buffer appendString: @"<params>"];
    
    if (myParameters) {
        NSEnumerator *enumerator = [myParameters objectEnumerator];
        id parameter = nil;
        
        while ((parameter = [enumerator nextObject])) {
            [buffer appendString: @"<param>"];
            [buffer appendString: [self encodeObject: parameter]];
            [buffer appendString: @"</param>"];
        }
    }
    
    [buffer appendString: @"</params>"];
    
    [buffer appendString: @"</methodCall>"];
    
    return buffer;
}

- (void)setMethod: (NSString *)method withParameters: (NSArray *)parameters {
	myMethod = method;
	myParameters = parameters;
}

- (NSString *)method {
    return myMethod;
}

- (NSArray *)parameters {
    return myParameters;
}

@end

@implementation XMLRPCDefaultRequestEncoder (XMLRPCEncoderPrivate)

- (NSString *)valueTag: (NSString *)tag value: (NSString *)value {
    return [NSString stringWithFormat: @"<value><%@>%@</%@></value>", tag, value, tag];
}

- (NSString *)replaceTarget: (NSString *)target withValue: (NSString *)value inString: (NSString *)string {
    return [[string componentsSeparatedByString: target] componentsJoinedByString: value];    
}

- (NSString *)encodeObject: (id)object {
    if (!object) {
        return nil;
    }
    
    if ([object isKindOfClass: [NSArray class]]) {
        return [self encodeArray: object];
    } else if ([object isKindOfClass: [NSDictionary class]]) {
        return [self encodeDictionary: object];
    } else if (((__bridge CFBooleanRef)object == kCFBooleanTrue) || ((__bridge CFBooleanRef)object == kCFBooleanFalse)) {
        return [self encodeBoolean: (CFBooleanRef)object];
    } else if ([object isKindOfClass: [NSNumber class]]) {
        return [self encodeNumber: object];
    } else if ([object isKindOfClass: [NSString class]]) {
        return [self encodeString: object omitTag: NO];
    } else if ([object isKindOfClass: [NSDate class]]) {
        return [self encodeDate: object];
    } else if ([object isKindOfClass: [NSData class]]) {
        return [self encodeData: object];
    } else {
        return [self encodeString: object omitTag: NO];
    }
}

- (NSString *)encodeArray: (NSArray *)array {
    NSMutableString *buffer = [NSMutableString string];
    NSEnumerator *enumerator = [array objectEnumerator];
    
    [buffer appendString: @"<value><array><data>"];
    
    id object = nil;
    
    while (object = [enumerator nextObject]) {
        [buffer appendString: [self encodeObject: object]];
    }
    
    [buffer appendString: @"</data></array></value>"];
    
    return (NSString *)buffer;
}

- (NSString *)encodeDictionary: (NSDictionary *)dictionary {
    NSMutableString * buffer = [NSMutableString string];
    NSEnumerator *enumerator = [dictionary keyEnumerator];
    
    [buffer appendString: @"<value><struct>"];
    
    NSString *key = nil;
    NSObject *val;
    
    while (key = [enumerator nextObject]) {
        [buffer appendString: @"<member>"];
        [buffer appendFormat: @"<name>%@</name>", [self encodeString: key omitTag: YES]];

        val = [dictionary objectForKey: key];
        if (val != [NSNull null]) {
            [buffer appendString: [self encodeObject: val]];
        } else {
            [buffer appendString:@"<value><nil/></value>"];
        }

        [buffer appendString: @"</member>"];
    }
    
    [buffer appendString: @"</struct></value>"];
    
    return (NSString *)buffer;
}

- (NSString *)encodeBoolean: (CFBooleanRef)boolean {
    if (boolean == kCFBooleanTrue) {
        return [self valueTag: @"boolean" value: @"1"];
    } else {
        return [self valueTag: @"boolean" value: @"0"];
    }
}

- (NSString *)encodeNumber: (NSNumber *)number {
    NSString *numberType = [NSString stringWithCString: [number objCType] encoding: NSUTF8StringEncoding];
    
    if ([numberType isEqualToString: @"d"]) {
        return [self valueTag: @"double" value: [number stringValue]];
    } else {
        return [self valueTag: @"i4" value: [number stringValue]];
    }
}

- (NSString *)encodeString: (NSString *)string omitTag: (BOOL)omitTag {
    return omitTag ? [string escapedString] : [self valueTag: @"string" value: [string escapedString]];
}

- (NSString *)encodeDate: (NSDate *)date {
    unsigned components = kCFCalendarUnitYear | kCFCalendarUnitMonth | kCFCalendarUnitDay | kCFCalendarUnitHour | kCFCalendarUnitMinute | kCFCalendarUnitSecond;
    NSDateComponents *dateComponents = [[NSCalendar currentCalendar] components: components fromDate: date];
    NSString *buffer = [NSString stringWithFormat: @"%.4ld%.2ld%.2ldT%.2ld:%.2ld:%.2ld", (long)[dateComponents year], (long)[dateComponents month], (long)[dateComponents day], (long)[dateComponents hour], (long)[dateComponents minute], (long)[dateComponents second], nil];
    
    return [self valueTag: @"dateTime.iso8601" value: buffer];
}

- (NSString *)encodeData: (NSData *)data {
    return [self valueTag: @"base64" value: [data base64EncodedString]];
}

@end
