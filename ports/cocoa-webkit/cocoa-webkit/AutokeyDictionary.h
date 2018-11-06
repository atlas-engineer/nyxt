//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//
//  AutokeyDictionary is a NSDictinoary that can
//  automatically insert new objects and returns
//  a unique key for them

#import <Foundation/Foundation.h>

@interface AutokeyDictionary : NSMutableDictionary
{
    NSMutableDictionary *_dict;
}

- (NSString *) insertElement:(NSObject *) object;
- (void) removeObjectForKey:(id) aKey;
- (NSArray *) allKeys;

@property (nonatomic, readwrite) NSInteger elementCount;

@end
