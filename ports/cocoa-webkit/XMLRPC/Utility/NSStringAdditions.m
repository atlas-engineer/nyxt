//
// Copyright (C) 2012 Eric Czarny <eczarny@gmail.com>
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "NSStringAdditions.h"

@implementation NSString (NSStringAdditions)

+ (NSString*)stringByGeneratingUUID {
    CFUUIDRef UUIDReference = CFUUIDCreate(nil);
    CFStringRef temporaryUUIDString = CFUUIDCreateString(nil, UUIDReference);
    CFRelease(UUIDReference);
    return (NSString*)CFBridgingRelease(temporaryUUIDString);
}

- (NSString*)unescapedString {
    NSMutableString* string = [NSMutableString stringWithString:self];

    [string replaceOccurrencesOfString:@"&amp;" withString:@"&" options:NSLiteralSearch range:NSMakeRange(0, [string length])];
    [string replaceOccurrencesOfString:@"&quot;" withString:@"\"" options:NSLiteralSearch range:NSMakeRange(0, [string length])];
    [string replaceOccurrencesOfString:@"&#x27;" withString:@"'" options:NSLiteralSearch range:NSMakeRange(0, [string length])];
    [string replaceOccurrencesOfString:@"&#x39;" withString:@"'" options:NSLiteralSearch range:NSMakeRange(0, [string length])];
    [string replaceOccurrencesOfString:@"&#x92;" withString:@"'" options:NSLiteralSearch range:NSMakeRange(0, [string length])];
    [string replaceOccurrencesOfString:@"&#x96;" withString:@"'" options:NSLiteralSearch range:NSMakeRange(0, [string length])];
    [string replaceOccurrencesOfString:@"&gt;" withString:@">" options:NSLiteralSearch range:NSMakeRange(0, [string length])];
    [string replaceOccurrencesOfString:@"&lt;" withString:@"<" options:NSLiteralSearch range:NSMakeRange(0, [string length])];

    return [NSString stringWithString:string];
}

- (NSString*)escapedString {
    NSMutableString* string = [NSMutableString stringWithString:self];

    // NOTE: we use unicode entities instead of &amp; &gt; &lt; etc. since some hosts (powweb, fatcow, and similar)
    // have a weird PHP/libxml2 combination that ignores regular entities
    [string replaceOccurrencesOfString:@"&" withString:@"&#38;" options:NSLiteralSearch range:NSMakeRange(0, [string length])];
    [string replaceOccurrencesOfString:@">" withString:@"&#62;" options:NSLiteralSearch range:NSMakeRange(0, [string length])];
    [string replaceOccurrencesOfString:@"<" withString:@"&#60;" options:NSLiteralSearch range:NSMakeRange(0, [string length])];

    return [NSString stringWithString:string];
}

@end
